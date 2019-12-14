#' @title Prepare everything the prediction model needs
model_init <- function(){
    predict_function <- function(model_object, new_data){
        stats::predict.lm(object = model_object, newdata = new_data, interval = params$interval) %>%
            as.data.frame(stringsAsFactors = FALSE) %>%
            dplyr::rename("fit" = "fit") %>%
            purrr::map_df(link_function)
    }

    link_function <- function(x) x

    model_config <- config::get(file = file.path(model_path, "model_config.yml"), use_parent = FALSE)

    list2env(model_config, envir = parent.frame())
    assign("predict_function", predict_function, envir = parent.frame())
    assign("link_function", link_function, envir = parent.frame())
    return(invisible())
}
