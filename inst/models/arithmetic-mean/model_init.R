#' @title Prepare everything the prediction model needs
model_init <- function(){
    params <- list()

    predict_function <- function(model_object, new_data){
        stats::predict.lm(object = model_object, newdata = new_data, interval = "prediction") %>%
            as.data.frame(stringsAsFactors = FALSE) %>%
            dplyr::rename("fit" = "fit")
    }

    link_function <- function(x) x

    assign('params', params, parent.frame())
    assign("predict_function", predict_function, envir = parent.frame())
    assign("link_function", link_function, envir = parent.frame())
    return(invisible())
}
