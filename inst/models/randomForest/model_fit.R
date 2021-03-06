#' @title Fit One or More Models to a Training Set
#' @param historical_data (`data.frame`) A table where samples are in rows and features are in columns.
#' @param model_formula (`formula`) A symbolic description of the model to be fitted.
#' @note \code{model_fit} has access to variables which were assigned to \code{model_environment}.
#' @return A model object
model_fit <- function(historical_data, model_formula)
{
    set.seed(1451)

    suppressWarnings(
        mdl_obj <- randomForest::randomForest(
            model_formula,
            data = historical_data,
            ntree = params$ntree,
            mtry = params$mtry
        )
    )

    return(mdl_obj)
}
