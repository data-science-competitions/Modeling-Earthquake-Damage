# Configuration -----------------------------------------------------------
options(verbose = TRUE)
enable_parallelism()

# Setup -------------------------------------------------------------------
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
role_none <- match_columns(tidy_data, "^geo_level_3_id$|^geo_level_3_id_in_")
role_input_1 <- match_columns(tidy_data, "_type$")
role_input_2 <- match_columns(tidy_data, "^has_superstructure_mud_mortar_stone$")
role_input_3 <- match_columns(tidy_data, "^geo_level_[1-3]_id_[cat]|^mfa_dim_")
role_input_4 <- match_columns(tidy_data, "^age$|_percentage$|^count_")
role_input <- unique(c(role_input_1, role_input_2, role_input_3, role_input_4))
role_target <- "damage_grade"

# Split the Data ----------------------------------------------------------
historical_data <- tidy_data %>% dplyr::filter(.set_source %in% "historical_data")
new_data <- tidy_data %>% dplyr::filter(.set_source %in% "new_data")

# Sample the Data ---------------------------------------------------------
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

# Create Submission -------------------------------------------------------
submission_path <- file.path(getOption("path_submissions"), paste0("(",model_name,")(",make.names(Sys.time()),").csv"))
dir.create(dirname(submission_path), showWarnings = FALSE, recursive = TRUE)
pm$response %>%
    dplyr::select(building_id, fit) %>%
    dplyr::mutate(fit = as_earthquake_damage(fit)) %>%
    dplyr::rename(damage_grade = fit) %>%
    readr::write_csv(submission_path)
