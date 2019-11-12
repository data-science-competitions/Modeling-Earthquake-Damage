#' @title Fit One or More Models to a Training Set
#' @param historical_data (`data.frame`) A table where samples are in rows and features are in columns.
#' @param model_formula (`formula`) A symbolic description of the model to be fitted.
#' @note \code{model_fit} has access to variables which were assigned to \code{model_environment}.
#' @return A model object
model_fit <- function(historical_data, model_formula)
{
    y <- all.vars(update(model_formula, .~0))
    X <- all.vars(update(model_formula, 0~.))

    historical_data.matrix <- as.matrix(historical_data)
    historical_data.xgb <- xgboost::xgb.DMatrix(
        historical_data.matrix[, X],
        label = historical_data.matrix[, y]
    )

    mdl_obj <- xgboost::xgb.train(
        params = params,
        data = historical_data.xgb,
        nrounds = params$nrounds
    )

    return(mdl_obj)
}
