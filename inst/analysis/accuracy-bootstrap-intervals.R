# Setup ------------------------------------------------------------------------
library(ggplot2)
ds <- DataStore$new()
model_name <- c("arithmetic-mean", "rpart", "ranger", "catboost")[4]
output_dir <- file.path(getOption("path_archive"), model_name)
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

# Get the Data -----------------------------------------------------------------
historical_data <- ds$data_model$historical_data

# Saple the Data ---------------------------------------------------------------
set.seed(1936)
K <- 30
rset_obj <- historical_data %>% dplyr::sample_n(6e4) %>% rsample::bootstraps(times = K, strata = "damage_grade")

# Formualte the Model ----------------------------------------------------------
role_pk <- "building_id"
role_none <- NULL
role_input <- match_columns(historical_data, "^geo_|^has_superstructure_mud_mortar_stone$|^age$|_type$")
role_target <- "damage_grade"

# Run model ---------------------------------------------------------------
model_performance <- tibble::tibble()
pb <- progress::progress_bar$new(total = K, format = "Training Models [:bar] :current/:total (:percent) eta: :eta")
for(k in seq_len(get_rsample_num_of_splits(rset_obj))){
    pb$tick()

    train_set <- get_rsample_training_set(rset_obj, split = k)
    test_set <- get_rsample_test_set(rset_obj, split = k)

    pm <- PentaModel$new(path = file.path(.Options$path_models, model_name))
    pm$set_historical_data(train_set)
    pm$set_new_data(test_set)
    pm$set_role_pk(role_pk)
    pm$set_role_input(role_input)
    pm$set_role_target(role_target)

    pm$model_init()$model_fit()$model_predict()$model_store()

    metadata <- test_set %>% dplyr::select(role_pk, dplyr::starts_with("geo_"))
    truth.numeric <- test_set %>% dplyr::select_at(c(role_pk, role_target)) %>% dplyr::rename("truth.numeric" = !!role_target)
    estimate.numeric <- pm$response %>% dplyr::select(role_pk, fit) %>% dplyr::rename("estimate.numeric" = "fit")
    data <-
        metadata %>%
        dplyr::right_join(truth.numeric, by = role_pk) %>%
        dplyr::right_join(estimate.numeric, by = role_pk) %>%
        dplyr::mutate(truth.class = as_earthquake_damage(truth.numeric), estimate.class = as_earthquake_damage(estimate.numeric))
    data <- data %>% dplyr::group_by(geo_level_1_id)

    new_entry <-
        Yardstick$
        new(data, truth = "truth.class", estimate = "estimate.class")$
        # delete_label(".estimator")$
        insert_label(".sample", k)$
        all_class_metrics

    model_performance <- dplyr::bind_rows(model_performance, new_entry)
}

# Visualisation -----------------------------------------------------------
boxplot_data <-
    model_performance %>%
    dplyr::mutate(geo_level_1_id = forcats::fct_reorder(geo_level_1_id, .estimate, median))
boxplot(.estimate ~ geo_level_1_id, boxplot_data %>% as.data.frame())

