# Helper Functions -------------------------------------------------------------
sample_the_data <- function(.data){
    .data %>%
        dplyr::group_by(damage_grade) %>%
        dplyr::sample_frac(size = 0.1) %>%
        rsample::initial_split(prop = 0.7, strata = "damage_grade")
}

# Setup ------------------------------------------------------------------------
ds <- DataStore$new()
model_name <- c("arithmetic-mean", "rpart")[1]
output_dir <- file.path(getOption("path_archive"), model_name)
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

# Get the Data -----------------------------------------------------------------
historical_data <-
    ds$data_model %>%
    dm::cdm_get_tables() %>%
    .$historical_data %>%
    as.data.frame(stringsAsFactors = FALSE)

# Sample the Data --------------------------------------------------------------
set.seed(1936)
rset_obj <- sample_the_data(historical_data)
role_pk <- "building_id"
role_none <- tidyselect::vars_select(names(historical_data), dplyr::starts_with("geo_"))
role_input <- tidyselect::vars_select(names(historical_data), dplyr::starts_with("has_"))
role_target <- "damage_grade"

train_set <-
    get_rsample_training_set(rset_obj, split = 1) %>%
    dplyr::select(role_pk, role_input, role_target, role_none)

test_set <-
    get_rsample_test_set(rset_obj, split = 1) %>%
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
truth <- test_set %>% dplyr::select_at(c(role_pk, role_target)) %>% dplyr::rename("truth" = !!role_target)
estimate <- pm$response %>% dplyr::select(role_pk, fit) %>% dplyr::rename("estimate" = "fit")
data <- dplyr::right_join(truth, estimate, by = role_pk)
metrics <- Yardstick$new(data, truth = "truth", estimate = "estimate")
metrics$delete_label(".estimator")
model_performance <- dplyr::bind_rows(metrics$rmse, metrics$mae, metrics$rsq, metrics$ccc)
