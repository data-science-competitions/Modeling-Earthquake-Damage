# Configuration -----------------------------------------------------------
options(verbose = FALSE)
enable_parallelism()

# Setup -------------------------------------------------------------------
fs <- FeatureStore$new()
model_name <- "data-driven-generalisation"
output_dir <- file.path(getOption("path_archive"), model_name)
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

# Get the Data ------------------------------------------------------------
tidy_data <-
    fs$tidy_data #%>%
    # dplyr::left_join(by = "building_id", fs$geo_features) %>%
    # dplyr::left_join(by = "building_id", fs$mfa_features) %>%
    # dplyr::left_join(by = "building_id", fs$age_features)
historical_data <- tidy_data %>% dplyr::filter(.set_source %in% "historical_data") %>% tibble::add_column(is_rollout = 0)
new_data <- tidy_data %>% dplyr::filter(.set_source %in% "new_data") %>% tibble::add_column(is_rollout = 1)

# Sample the Data ---------------------------------------------------------
role_pk <- "building_id"
role_none <- NULL
role_input_1 <- match_columns(historical_data, "_type$|^has_superstructure_")
role_input_2 <- match_columns(historical_data, "^geo_level_[1]_id$|^geo_level_[1-3]_id_[cat]|^mfa_dim_")
role_input_3 <- match_columns(historical_data, "^age$|_percentage$|^count_")
role_input <- unique(c(role_input_1, role_input_2, role_input_3))
role_target <- "is_rollout"

train_set <-
    historical_data %>%
    dplyr::select(-dplyr::starts_with(".")) %>%
    dplyr::select(role_pk, role_input, role_target, role_none)

test_set <-
    new_data %>%
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
