#' @title Predict New Observations on Fitted Model
#' @param new_data (`data.frame`) A table where samples are in rows and features are in columns.
#' @param model_object (`formula`) A model object which was created by \code{model_init}.
#' @return A vector of predictions
model_predict <- function(new_data, model_object)
{
    predict_function <- function(new_data, model_object){
        response <- predict(
            object = model_object,
            newdata = new_data,
            type = c("vector", "prob", "class", "matrix")[1]
        )
    }

    link_function <- function(x){ # 1 <= x <= 3
        minmax <- function(x, a, b) pmin(pmax(x, a), b)
        normalize <- function(x) (x - min(x)) / (max(x) - min(x))

        y <- x %>% minmax(1, 3) %>% scale() %>% normalize()
        y <- y * 2 + 1

        as.vector(y)
    }

    response <- new_data %>% predict_function(model_object) %>% link_function()
    return(response)
}
