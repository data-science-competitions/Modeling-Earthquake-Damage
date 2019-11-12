#' @title Predict New Observations on Fitted Model
#' @param new_data (`data.frame`) A table where samples are in rows and features are in columns.
#' @param model_object (`formula`) A model object which was created by \code{model_init}.
#' @return A vector of predictions
model_predict <- function(new_data, model_object)
{
    new_data.matrix <- as.matrix(new_data)
    new_data.xgb <- xgboost::xgb.DMatrix(
        data = new_data.matrix[, role_input],
        label = new_data.matrix[, role_target]
    )

    response <- predict_function(model_object, new_data.xgb)
    response <- apply(response, 2, link_function)
    return(response)
}
