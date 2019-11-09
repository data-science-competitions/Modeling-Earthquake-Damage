# Setup -------------------------------------------------------------------
ds <- DataStore$new()
model_name <- c("arithmetic-mean", "rpart", "ranger", "catboost")[4]
output_dir <- file.path(getOption("path_archive"), model_name)
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

# Get the Data ------------------------------------------------------------
historical_data <- ds$data_model$historical_data

# Sample the Data ---------------------------------------------------------
set.seed(1936)
rset_obj <- historical_data %>% rsample::initial_split(prop = 0.8, strata = "damage_grade")
role_pk <- "building_id"
role_none <- NULL
role_input <- match_columns(historical_data, "^geo_|^has_")
role_target <- "damage_grade"

train_set <-
    get_rsample_training_set(rset_obj, split = 1) %>%
    dplyr::sample_n(6e4) %>%
    dplyr::select(role_pk, role_input, role_target, role_none)

test_set <-
    get_rsample_test_set(rset_obj, split = 1) %>%
    dplyr::sample_n(4e4) %>%
    dplyr::select(role_pk, role_input, role_target, role_none)

# Run model ---------------------------------------------------------------
pm <- PentaModel$new(path = file.path(.Options$path_models, model_name))
pm$set_historical_data(train_set)
pm$set_new_data(test_set)
pm$set_role_pk(role_pk)
pm$set_role_input(role_input)
pm$set_role_target(role_target)

pm$model_init()
pm$model_fit()
pm$model_predict()
pm$model_store()

# Evaluate Model ----------------------------------------------------------
metadata <- test_set %>% dplyr::select(role_pk, dplyr::starts_with("geo_"))
truth.numeric <- test_set %>% dplyr::select_at(c(role_pk, role_target)) %>% dplyr::rename("truth.numeric" = !!role_target)
estimate.numeric <- pm$response %>% dplyr::select(role_pk, fit) %>% dplyr::rename("estimate.numeric" = "fit")
data <-
    metadata %>%
    dplyr::right_join(truth.numeric, by = role_pk) %>%
    dplyr::right_join(estimate.numeric, by = role_pk) %>%
    dplyr::mutate(truth.class = as_earthquake_damage(truth.numeric), estimate.class = as_earthquake_damage(estimate.numeric))
data <- data %>% dplyr::group_by(geo_level_1_id)

model_class_performance <-
    Yardstick$
    new(data, truth = "truth.class", estimate = "estimate.class")$
    delete_label(".estimator")$
    insert_label(".model", pm$model_name)$
    all_class_metrics

model_numeric_performance <-
    Yardstick$
    new(data, truth = "truth.numeric", estimate = "estimate.numeric")$
    delete_label(".estimator")$
    insert_label(".model", pm$model_name)$
    all_numeric_metrics

model_performance <- dplyr::bind_rows(model_class_performance, model_numeric_performance)
print(model_performance)


# Visualisation -----------------------------------------------------------
accuracy <- model_performance %>% dplyr::filter(.metric %in% "accuracy")
grand_accuracy <- sum(accuracy$.estimate * accuracy$.n) / sum(accuracy$.n)
## Metrics Correlation Plot
model_performance %>%
    dplyr::mutate(.metric = paste0("metric_", .metric)) %>%
    tidyr::spread(".metric", ".estimate") %>%
    dplyr::select(dplyr::starts_with("metric_")) %>%
    dplyr::rename_all(function(x) stringr::str_remove_all(x, "metric_")) %>%
    as.matrix() %>%
    PerformanceAnalytics::chart.Correlation(method = "spearman", histogram = FALSE)
## Density Plot
par(pty = "m")
accuracy %>% .$.estimate %>% density(from = 0, to = 1) %>% plot(main = "")
title("Accuracy ~ geo_level_1_id Density Plot")
abline(v = grand_accuracy, lty = 2, col = "gray")
## Scatter Plot
par(pty = "s")
accuracy %>% dplyr::select(.n, .estimate) %>% plot(ylim = c(0.5, 1), pch = 21, cex = 1, bg = "orange", col = "gray")
title("Accuracy ~ geo_level_1_id Scatter Plot")
abline(h = grand_accuracy, lty = 2, col = "gray")
with(accuracy, text(.estimate ~ .n, labels = geo_level_1_id, pos = 4, cex = 0.5))
