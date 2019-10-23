#' @title Prepare everything the prediction model needs
model_init <- function(){
    params <- list()
    params$maxdepth <- 3
    params$minsplit <- 10
    params$maxcompete <- 2

    assign('params', params, parent.frame())
    return(invisible())
}
