#' @title Fit One or More Models to a Training Set
#' @param historical_data (`data.frame`) A table where samples are in rows and features are in columns.
#' @param model_formula (`formula`) a symbolic description of the model to be fitted.
model_fit <- function(historical_data, model_formula)
{
    mdl_obj <- rpart::rpart(
        model_formula,
        data = historical_data,
        method = "anova",
        control = rpart_control
    )

    return(mdl_obj)
}
