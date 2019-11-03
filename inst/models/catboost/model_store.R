#' @title Log the Results
#' @description Log the results (predictions and model objects) in a concise and
#'   consistent manner.
model_store <- function(){
    target <- file.path(getOption("path_archive", default = tempdir()), model_name)
    source <- params$train_dir

    dir.create(target, showWarnings = FALSE, recursive = TRUE)
    file.copy(source, target, recursive = TRUE)

    return(invisible())
}
