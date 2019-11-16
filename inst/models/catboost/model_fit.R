#' @title Fit One or More Models to a Training Set
#' @param historical_data (`data.frame`) A table where samples are in rows and features are in columns.
#' @param model_formula (`formula`) A symbolic description of the model to be fitted.
#' @note \code{model_fit} has access to variables which were assigned to \code{model_environment}.
#' @return A model object
model_fit <- function(historical_data, model_formula)
{
    historical_data <-
        catboost::catboost.load_pool(
            data = historical_data %>% dplyr::select(role_input),
            label = historical_data %>% dplyr::select(role_target) %>% unlist()
        )

    params$logging_level = ifelse(getOption("verbose"), "Verbose", "Silent")
    mdl_obj <- catboost::catboost.train(learn_pool = historical_data, params = params)

    return(mdl_obj)
}
