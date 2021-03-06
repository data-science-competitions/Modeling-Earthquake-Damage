# Setup -------------------------------------------------------------------
options(verbose = FALSE)
fs <- FeatureStore$new()
model_names <- c(
    "arithmetic-mean", # [1]
    "catboost",        # [2]
    "randomForest",    # [3]
    "ranger",          # [4]
    "rpart",           # [5]
    "xgboost"          # [6]
)[c(2,4,6)]
output_dir <- file.path(getOption("path_archive"), "ensemble")
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

# Get the Data ------------------------------------------------------------
tidy_data <-
    fs$tidy_data %>%
    dplyr::left_join(by = "building_id", fs$geo_features) %>%
    dplyr::left_join(by = "building_id", fs$age_features)
historical_data <- tidy_data %>% dplyr::filter(.set_source %in% "historical_data")
new_data <- tidy_data %>% dplyr::filter(.set_source %in% "new_data")

# Sample the Data ---------------------------------------------------------
role_pk <- "building_id"
role_none <- NULL
role_input_1 <- match_columns(historical_data, "^geo_level_")
role_input_2 <- match_columns(historical_data, "^age")
role_input_3 <- match_columns(historical_data, "^has_superstructure_mud_mortar_stone$|_type$")
role_input <- c(role_input_1, role_input_2, role_input_3)
role_target <- "damage_grade"

train_set <-
    historical_data %>%
    dplyr::filter(.set_role %in% "train") %>%
    dplyr::select(-dplyr::starts_with(".")) %>%
    dplyr::select(role_pk, role_input, role_target, role_none)

test_set <-
    historical_data %>%
    dplyr::filter(.set_role %in% "validation") %>%
    dplyr::select(-dplyr::starts_with(".")) %>%
    dplyr::select(role_pk, role_input, role_target, role_none)

# Fit models --------------------------------------------------------------
predictions <- test_set %>% dplyr::select(building_id)
for(model_name in model_names){
    message("Fitting ", model_name)
    pm <- PentaModel$new(path = file.path(.Options$path_models, model_name))
    pm$set_historical_data(train_set)
    pm$set_new_data(test_set)
    pm$set_role_pk(role_pk)
    pm$set_role_input(role_input)
    pm$set_role_target(role_target)
    pm$model_init()$model_fit()$model_predict()
    predictions <- dplyr::left_join(
        predictions,
        pm$response %>% dplyr::select(!!role_pk, fit) %>% dplyr::rename(!!model_name := "fit"),
        by = "building_id"
    )
}

# Ensemble Models ---------------------------------------------------------
blend <- predictions %>% tibble::add_column(fit = rowMeans(predictions[,-1]))

# Visualise Results -------------------------------------------------------
plot_path <- file.path(output_dir, "(correlation plot)(predictions).jpg")
PerformanceAnalytics::chart.Correlation(predictions[, -1], histogram=TRUE, pch=19)
title("Prediction Correlation Plot")
dev.print(jpeg, plot_path, width = 800)

plot_path <- file.path(output_dir, "(correlation plot)(residuals).jpg")
residuals <- as.matrix(predictions[, -1]) - test_set[[role_target]]
PerformanceAnalytics::chart.Correlation(residuals, histogram=TRUE, pch=19)
title("Residuals Correlation Plot")
dev.print(jpeg, plot_path, width = 800)

# Evaluate Model ----------------------------------------------------------
data <-
    blend %>%
    dplyr::left_join(pm$response, by = role_pk) %>%
    dplyr::left_join(blend, by = role_pk) %>%
    dplyr::rename("truth.numeric" = !!role_target, "estimate.numeric" = "fit") %>%
    dplyr::mutate(truth.class = as_earthquake_damage(truth.numeric), estimate.class = as_earthquake_damage(estimate.numeric)) %>%
    dplyr::group_by(geo_level_1_id)

## Ungrouped Evaluation (overview)
Yardstick$
    new(data %>% dplyr::ungroup(), truth = "truth.class", estimate = "estimate.class")$
    set_estimator("micro")$
    insert_label(".model", pm$model_name)$
    f_meas

## Grouped Evaluation (drill-down)
model_class_performance <-
    Yardstick$
    new(data, truth = "truth.class", estimate = "estimate.class")$
    set_estimator("micro")$
    insert_label(".model", pm$model_name)$
    all_class_metrics

model_numeric_performance <-
    Yardstick$
    new(data, truth = "truth.numeric", estimate = "estimate.numeric")$
    insert_label(".model", pm$model_name)$
    all_numeric_metrics

model_performance <- dplyr::bind_rows(model_class_performance, model_numeric_performance)
print(model_performance)
