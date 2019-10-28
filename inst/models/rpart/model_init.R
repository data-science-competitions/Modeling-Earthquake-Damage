#' @title Prepare everything the prediction model needs
model_init <- function(){
    params <- list()
    params$maxdepth <- 3
    params$minsplit <- 10
    params$maxcompete <- 2

    predict_function <- function(model_object, new_data){
        predict(object = model_object, newdata = new_data)
    }

    link_function <- function(x){ # 1 <= x <= 3
        minmax <- function(x, a, b) pmin(pmax(x, a), b)
        normalize <- function(x) if(max(x) == min(x)) x else (x - min(x)) / (max(x) - min(x))
        scale <- function(x) if(isTRUE(x %>% sd() > 0)) base::scale(x) else base::scale(x, TRUE, FALSE)

        y <- x %>% minmax(1, 3) %>% scale() %>% normalize()
        y <- y * 2 + 1

        as.vector(y)
    }

    assign('params', params, parent.frame())
    assign("predict_function", predict_function, envir = parent.frame())
    assign("link_function", link_function, envir = parent.frame())
    return(invisible())
}
