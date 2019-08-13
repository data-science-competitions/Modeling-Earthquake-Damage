# Helper-Functions -------------------------------------------------------------
sample_the_data <- function(.data){
    .data %>%
        dplyr::group_by(damage_grade) %>%
        dplyr::sample_frac(size = 0.1) %>%
        rsample::initial_split(prop = 0.7, strata = "damage_grade")
}

compose_formula <- function(role_pk = NULL, role_none = NULL, role_input, role_target){
    X <- role_input %>% setdiff(role_none) %>% setdiff(role_pk)
    y <- role_target

    formula(paste(y, "~", paste(X, collapse = " + ")))
}

minmax <- function(x, lb, ub) {
    stopifnot(lb < ub)
    .minmax <- function(x, lb, ub) min(ub, max(x, lb))
    sapply(x, .minmax, lb = lb, ub = ub)
}

set.seed(1936)
historical_data <- DataStore$new()$data_model %>% dm::cdm_get_tables() %>% .$historical_data
rset_obj <- sample_the_data(historical_data)

# Sample the Data --------------------------------------------------------------
#' <https://www.ibm.com/support/knowledgecenter/en/SSLVMB_23.0.0/spss/base/dataedit_roles.html>
#' role_target ~ role_input - role_none | role_pk
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

# Fit Model --------------------------------------------------------------------
mdl_formula <- compose_formula(role_pk, role_none, role_input, role_target)
mdl_obj <- lm(mdl_formula, data = train_set)
predict_function <- function(X, m) predict.lm(m, newdata = X)
link_function <- function(x) x %>% round() %>% minmax(lb = 1, ub = 3)

response <- predict_function(X = test_set, m = mdl_obj) %>% link_function()
names(response) <- test_set[[role_pk]]
response <- tibble::enframe(response, name = role_pk, value = role_target)

# Model Evaluation -------------------------------------------------------------
median_value <- train_set[[role_target]] %>% median() %>% rep(nrow(test_set))

base_err_f1 <- Metrics::f1(actual = test_set[[role_target]], predicted = median_value)
mdl_err_f1 <- Metrics::f1(actual = test_set[[role_target]], predicted = response[[role_target]])

base_err_rmse <- Metrics::rmse(actual = test_set[[role_target]], predicted = median_value)
mdl_err_rmse <- Metrics::rmse(actual = test_set[[role_target]], predicted = response[[role_target]])

(err_ratio_rf <- mdl_err_f1 / base_err_f1)
(err_ratio_rmse <- mdl_err_rmse / base_err_rmse)
