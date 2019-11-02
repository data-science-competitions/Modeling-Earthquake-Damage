#' @title Fit One or More Models to a Training Set
#' @param historical_data (`data.frame`) A table where samples are in rows and features are in columns.
#' @param model_formula (`formula`) A symbolic description of the model to be fitted.
#' @note \code{model_fit} has access to variables which were assigned to \code{model_environment}.
#' @return A model object
model_fit <- function(historical_data, model_formula)
{

    mdl_obj <- ranger::ranger(
        model_formula,
        data = historical_data,
        num.trees = params$num.trees,
        mtry = eval(parse(text=params$mtry))
    )

    return(mdl_obj)
}
