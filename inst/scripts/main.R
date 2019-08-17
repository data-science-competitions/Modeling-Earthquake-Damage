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
role_pk <- "building_id" # private key
role_none <- c(
    tidyselect::vars_select(names(historical_data), dplyr::starts_with("geo_")),
    tidyselect::vars_select(names(historical_data), dplyr::starts_with("has_"))
)
role_input <- names(historical_data)
role_target <- "damage_grade"

train_set <-
    get_rsample_training_set(rset_obj, split = 1) %>%
    dplyr::select(role_pk, role_input, role_target, -role_none)

test_set <-
    get_rsample_test_set(rset_obj, split = 1) %>%
    dplyr::select(role_pk, role_input, role_target, -role_none)

# Run model ---------------------------------------------------------------
pm <- PentaModel$new(path = file.path(.Options$path_models, "rpart-tree"))
