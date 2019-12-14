# Configuration -----------------------------------------------------------
options(verbose = FALSE)
enable_parallelism()

# Setup -------------------------------------------------------------------
fs <- FeatureStore$new()
model_name <- c(
    "arithmetic-mean", # [1]
    "rpart",           # [2]
    "ranger",          # [3]
    "catboost",        # [4]
    "randomForest",    # [5]
    "xgboost"          # [6]
)[6]
output_dir <- file.path(getOption("path_archive"), model_name)
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

# Get the Data ------------------------------------------------------------
tidy_data <-
    fs$tidy_data %>%
    dplyr::left_join(by = "building_id", fs$geo_features) %>%
    dplyr::left_join(by = "building_id", fs$mfa_features) %>%
    dplyr::left_join(by = "building_id", fs$age_features)

# Features Selection ------------------------------------------------------
role_pk <- "building_id"
role_none <- "geo_level_3_id"
role_input_1 <- match_columns(tidy_data, "_type$")
role_input_2 <- match_columns(tidy_data, "^has_superstructure_mud_mortar_stone$")
role_input_3 <- match_columns(tidy_data, "^geo_level_[1-3]_id_[cat]|^mfa_dim_")
role_input_4 <- match_columns(tidy_data, "^age$|_percentage$|^count_")
role_input <- unique(c(role_input_1, role_input_2, role_input_3, role_input_4))
role_target <- "damage_grade"

# Sample the Data ---------------------------------------------------------
set.seed(1715)
rset_obj <-
    tidy_data %>%
    dplyr::filter(.set_source %in% "historical_data") %>%
    dplyr::sample_n(1e5) %>%
    rsample::group_vfold_cv(group = "geo_level_3_id", v = 2)

# Run model ---------------------------------------------------------------
response <- tibble::tibble()
K <- get_rsample_num_of_splits(rset_obj)
pb <- progress::progress_bar$new(total = K, format = "Training model [:bar] :current/:total (:percent) eta: :eta")
for(k in seq_len(K)){
    train_set <-
        get_rsample_training_set(rset_obj, k) %>%
        dplyr::select(-dplyr::starts_with(".")) %>%
        dplyr::select(role_pk, role_input, role_target, role_none)

    test_set <-
        get_rsample_test_set(rset_obj, k) %>%
        dplyr::select(-dplyr::starts_with(".")) %>%
        dplyr::select(role_pk, role_input, role_target, role_none)

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

    response <- response %>% dplyr::bind_rows(pm$response %>% tibble::add_column(split = k, .before = TRUE))
    pb$tick()
}
pm$model_end()

# Evaluate Model ----------------------------------------------------------
data <-
    test_set %>%
    dplyr::left_join(response, by = role_pk) %>%
    dplyr::rename("truth.numeric" = !!role_target, "estimate.numeric" = "fit") %>%
    dplyr::mutate(truth.class = as_earthquake_damage(truth.numeric), estimate.class = as_earthquake_damage(estimate.numeric))

## Ungrouped Evaluation (overview)
Yardstick$
    new(data %>% dplyr::ungroup(), truth = "truth.class", estimate = "estimate.class")$
    set_estimator("micro")$
    insert_label(".model", pm$model_name)$
    f_meas
