# Helper Functions -------------------------------------------------------------
sample_the_data <- function(.data){
    .data %>%
        dplyr::group_by(damage_grade) %>%
        dplyr::sample_frac(size = 0.1) %>%
        rsample::initial_split(prop = 0.7, strata = "damage_grade")
}

# Get the Data -----------------------------------------------------------------
historical_data <-
    DataStore$new()$data_model %>%
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
pm <- PentaModel$new(path = file.path(.Options$path_models, "rpart-tree"))
pm$set_historical_data(train_set)
pm$set_new_data(test_set)
pm$set_role_pk(role_pk)
pm$set_role_input(role_input)
pm$set_role_target(role_target)

pm$model_init()
pm$model_fit()
pm$model_predict()

# Evaluate Model ----------------------------------------------------------
truth <- test_set %>% dplyr::select(role_pk, role_target)
predicted <- pm$response

ls(pm, all.names=TRUE)
