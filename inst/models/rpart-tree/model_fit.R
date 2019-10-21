#' @title Fit One or More Models to a Training Set
#' @param historical_data (`data.frame`) A table where samples are in rows and features are in columns.
#' @param model_formula (`formula`) a symbolic description of the model to be fitted.
#' @note \code{model_fit} has access to variables which were assigned to \code{model_environment}.
model_fit <- function(historical_data, model_formula)
{
    rpart_control <- rpart::rpart.control(
        maxdepth = params$maxdepth,
        minsplit = params$minsplit,
        maxcompete = params$minsplit
    )

    mdl_obj <- rpart::rpart(
        model_formula,
        data = historical_data,
        method = "anova",
        control = rpart_control
    )

    return(mdl_obj)
}
