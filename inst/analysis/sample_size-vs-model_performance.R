# Helper Functions --------------------------------------------------------
logseq <- function(from, to, ...) 10^(seq(log10(from), log10(to), ...))

# Setup ------------------------------------------------------------------------
ds <- DataStore$new()
model_name <- c("arithmetic-mean", "rpart", "ranger", "catboost")[4]
output_dir <- file.path(getOption("path_archive"), model_name)
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

# Get the Data -----------------------------------------------------------------
historical_data <- ds$data_model$historical_data

# Sample the Data --------------------------------------------------------------
set.seed(1936)
rset_obj <- historical_data %>% rsample::initial_split(prop = 0.7, strata = "damage_grade")
role_pk <- "building_id"
role_none <- NULL
role_input <- match_columns(historical_data, "^geo_|^has_")
role_target <- "damage_grade"

train_set <- get_rsample_training_set(rset_obj, split = 1)
test_set <- get_rsample_test_set(rset_obj, split = 1)

# Run model ---------------------------------------------------------------
model_performance <- tibble::tibble()
sample_sizes <- logseq(from = 1000, to = nrow(train_set), length.out = 10) %>% floor()
for(sample_size in sample_sizes){
    message("Training model on ", sample_size, "observations")
    pm <- PentaModel$new(path = file.path(.Options$path_models, model_name))
    pm$set_historical_data(train_set[1:sample_size, ])
    pm$set_new_data(test_set)
    pm$set_role_pk(role_pk)
    pm$set_role_input(role_input)
    pm$set_role_target(role_target)

    pm$model_init()$model_fit()$model_predict()$model_store()

    # Evaluate Model ----------------------------------------------------------
    metadata <- test_set %>% dplyr::select(role_pk, dplyr::matches("^geo_"))
    truth.numeric <- test_set %>% dplyr::select_at(c(role_pk, role_target)) %>% dplyr::rename("truth.numeric" = !!role_target)
    estimate.numeric <- pm$response %>% dplyr::select(role_pk, fit) %>% dplyr::rename("estimate.numeric" = "fit")
    data <-
        metadata %>%
        dplyr::right_join(truth.numeric, by = role_pk) %>%
        dplyr::right_join(estimate.numeric, by = role_pk) %>%
        dplyr::mutate(truth.class = as_earthquake_damage(truth.numeric), estimate.class = as_earthquake_damage(estimate.numeric))

    new_entry <-
        Yardstick$
        new(data, truth = "truth.class", estimate = "estimate.class")$
        delete_label(".estimator")$
        insert_label(".n_train", sample_size)$
        insert_label(".model", pm$model_name)$
        all_class_metrics %>%
        dplyr::rename(".n_test" = ".n") %>%
        dplyr::select(dplyr::matches("^.n"), dplyr::everything())

    model_performance <- dplyr::bind_rows(model_performance, new_entry)
}

# Visualisation -----------------------------------------------------------
accuracy <- model_performance %>% dplyr::filter(.metric %in% "accuracy")
par(pty = "m")
accuracy %>% dplyr::select(.n_train, .estimate) %>% plot(type = "b", xaxt = "n")
axis(1, at = seq(0, sample_sizes %>% max() %>% signif(2), length.out = 10), las = 2)
