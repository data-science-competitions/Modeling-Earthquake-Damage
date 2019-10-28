#' @title Prepare everything the prediction model needs
model_init <- function(){
    params <- list()

    assign('params', params, parent.frame())
    return(invisible())
}
