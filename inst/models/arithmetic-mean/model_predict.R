#' @title Predict New Observations on Fitted Model
#' @param new_data (`data.frame`) A table where samples are in rows and features are in columns.
#' @param model_object (`formula`) A model object which was created by \code{model_init}.
#' @return A vector of predictions
model_predict <- function(new_data, model_object)
{
    predict_function <- function(model_object, new_data){
        predict(object = model_object, newdata = new_data)
    }

    link_function <- function(x){ # 0 <= x < +Inf
        x %>% ReLU()
    }

    assign("link_function", link_function, envir = parent.frame())
    assign("predict_function", predict_function, envir = parent.frame())

    data.frame(response = predict_function(model_object, new_data) %>% link_function())
}
