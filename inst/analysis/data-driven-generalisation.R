# Instruction -------------------------------------------------------------
# 1. Copy paste role_input from main.R to this script
# 2. Run the script
# 3. Update main.R role_input
# 4. Repeat 1-3

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
    fs$tidy_data %>%
    mutate(is_rollout = as.numeric(.set_role %in% "test")) %>%
    dplyr::left_join(by = "building_id", fs$geo_features) %>%
    dplyr::left_join(by = "building_id", fs$mfa_features) %>%
    dplyr::left_join(by = "building_id", fs$age_features)

# Sample the Data ---------------------------------------------------------
role_pk <- "building_id"
role_none <- NULL
role_input_1 <- match_columns(tidy_data, "_type$|^has_superstructure_")
role_input_2 <- match_columns(tidy_data, "^geo_level_[1]_id$|^geo_level_[1-3]_id_[cat]|^mfa_dim_")
role_input_3 <- match_columns(tidy_data, "^age$|_percentage$|^count_")
role_input <- unique(c(role_input_1, role_input_2, role_input_3))
role_target <- "is_rollout"

set.seed(1423)
rset_obj <- rsample::initial_split(tidy_data, prop = 3/4)
train_set <-
    get_rsample_training_set(rset_obj, 1) %>%
    dplyr::select(-dplyr::starts_with(".")) %>%
    dplyr::select(role_pk, role_input, role_target, role_none)
test_set <-
    get_rsample_test_set(rset_obj, 1) %>%
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
data <-
    test_set %>%
    dplyr::left_join(pm$response, by = role_pk) %>%
    dplyr::rename("truth.numeric" = !!role_target, "estimate.numeric" = "fit")

## Ungrouped Evaluation (overview)
Yardstick$new(data %>% dplyr::ungroup(), truth = "truth.numeric", estimate = "estimate.numeric")$all_numeric_metrics
cor(data$estimate.numeric, data$truth.numeric)

## Visualisation
boxplot(estimate.numeric ~ truth.numeric, data)
