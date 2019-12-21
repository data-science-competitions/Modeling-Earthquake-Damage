# Configuration -----------------------------------------------------------
options(verbose = FALSE)
enable_parallelism()

# Setup -------------------------------------------------------------------
fs <- FeatureStore$new()
model_name <- c(
    "arithmetic-mean",        # [1]
    "rpart",                  # [2]
    "ranger",                 # [3]
    "catboost",               # [4]
    "randomForest",           # [5]
    "xgboost-regression",     # [6]
    "xgboost-classification"  # [7]
)[7]
output_dir <- file.path(getOption("path_archive"), model_name)
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

# Get the Data ------------------------------------------------------------
tidy_data <-
    fs$tidy_data %>%
    dplyr::left_join(by = "building_id", fs$geo_features) %>%
    dplyr::left_join(by = "building_id", fs$mfa_features) %>%
    dplyr::left_join(by = "building_id", fs$age_features)

# Get Variables Names -----------------------------------------------------
yaml_path <- file.path(getOption("path_models"), "model-variables.yml")
yaml_list <- yaml::read_yaml(yaml_path, eval.expr = TRUE)
list2env(yaml_list$default %>% lapply(merge_elements), envir = globalenv())

# Sample the Data ---------------------------------------------------------
historical_data <- tidy_data %>% dplyr::filter(.set_source %in% "historical_data")
new_data <- tidy_data %>% dplyr::filter(.set_source %in% "new_data")

# Split the Data ----------------------------------------------------------
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
pm$model_end()

# Evaluate Model ----------------------------------------------------------
pm$set_new_data(train_set)
response_train <- pm$model_predict()$response
pm$set_new_data(test_set)
response_test <- pm$model_predict()$response
data <-
    dplyr::bind_rows(train = response_train, test = response_test, .id = ".set") %>%
    dplyr::left_join(historical_data, by = role_pk) %>%
    dplyr::rename("truth.numeric" = !!role_target, "estimate.numeric" = "fit") %>%
    dplyr::mutate(truth.class = as_earthquake_damage(truth.numeric), estimate.class = as_earthquake_damage(estimate.numeric))
## Ungrouped Evaluation (overview)
Yardstick$
    new(data %>% dplyr::group_by(.set), truth = "truth.class", estimate = "estimate.class")$
    set_estimator("micro")$
    insert_label(".model", pm$model_name)$
    f_meas %>% dplyr::arrange(.set)

## Confusion Matrix
confusion_matrices <- yardstick::conf_mat(data %>% dplyr::group_by(.set), truth = truth.class, estimate = estimate.class)
gg_objects <- list()
for(l in seq_along(confusion_matrices$conf_mat)){
    set_name <- confusion_matrices[[".set"]][[l]]
    confusion_matrix <- confusion_matrices$conf_mat[[l]]

    gg_objects[[l]] <-
        confusion_matrix %>%
        ggplot2::autoplot(type = "heatmap") +
        ggplot2::ggtitle(set_name) +
        ggplot2::coord_fixed(ratio = 1)
}
do.call(gridExtra::grid.arrange, c(gg_objects, ncol = 2, nrow = 1, as.table = FALSE))
