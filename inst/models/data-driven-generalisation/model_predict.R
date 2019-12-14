#' @title Predict New Observations on Fitted Model
#' @param new_data (`data.frame`) A table where samples are in rows and features are in columns.
#' @param model_object (`formula`) A model object which was created by \code{model_init}.
#' @return A vector of predictions
model_predict <- function(new_data, model_object)
{
    response <- predict_function(model_object, new_data)
    response <- purrr::map_df(response, link_function)
    return(response)
}
