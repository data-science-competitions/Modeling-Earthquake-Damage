# PentaModel -------------------------------------------------------------------
#
#' @title A PentaModel Class
#'
#' @description NA
#'
#' @return (`PentaModel`) A DataStore with data tables.
#' @export
#'
#' @examples
#' \dontrun{mdl <- PentaModel$new()}
#'
PentaModel <- R6::R6Class(
    classname = "PentaModel",
    public = list(
        ## Public Methods
        initialize = function(path, env = as.environment(-1))
        {
            private$.model_path <- path
            private$.model_name <- basename(path)
            private$.env <- env
            private$.component_paths <- file.path(private$.model_path, paste0(private$.component_names,".R"))

            .load_model_components(private)
        },
        model_init = function() base::get("model_init", envir = private$.env)(),
        model_fit = function() base::get("model_fit", envir = private$.env)(),
        model_predict = function() base::get("model_predict", envir = private$.env)(),
        model_store = function() base::get("model_store", envir = private$.env)(),
        model_end = function() base::get("model_end", envir = private$.env)()
    ),

    private = list(
        .component_names = c("model_init", "model_fit", "model_predict", "model_store", "model_end"),
        .component_paths = character(0),
        .model_name = character(0),
        .model_path = character(0),
        .env = environment()
    ),

    active = list(
        model_name = function() private$.model_name,
        model_path = function() private$.model_path
    )
)

# Private Methods --------------------------------------------------------------
.load_model_components <- function(object){
    .assert_all_components_files_exist(object)
    .remove_model_components_from_env(object)
    sapply(object$.component_paths, source, local = object$.env)
    .assert_all_components_are_in_env(object)
}

.assert_all_components_files_exist <- function(object){
    assertive::assert_all_are_existing_files(object$.component_paths)
}

.assert_all_components_are_in_env <- function(object){
    function_names_in_env <- utils::lsf.str(envir = object$.env)
    assertive::assert_all_are_true(object$.component_names %in% function_names_in_env)
}

.remove_model_components_from_env <- function(object){
    suppressWarnings(rm(list = object$.component_names, envir = object$.env))
}
