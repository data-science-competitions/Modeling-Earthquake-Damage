# PentaModel -------------------------------------------------------------------
#
#' @title A PentaModel Class
#'
#' @description NA
#'
#' @return (`PentaModel`) A PentaModel API.
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
        model_init = function() .model_init(private),
        model_fit = function() .model_fit(private),
        model_predict = function() base::get("model_predict", envir = private$.env)(),
        model_store = function() base::get("model_store", envir = private$.env)(),
        model_end = function() base::get("model_end", envir = private$.env)(),
        set_historical_data = function(historical_data) .set_historical_data(private, historical_data),
        set_new_data = function(new_data) .set_new_data(private, new_data),
        set_model = function(model_object) .set_model(private, model_object)
    ),
    private = list(
        .component_names = c("model_init", "model_fit", "model_predict", "model_store", "model_end"),
        .component_paths = character(0),
        .model_name = character(0),
        .model_path = character(0),
        .model_object = NULL,
        .env = environment(),
        .historical_data = data.frame(),
        .new_data = data.frame()
    ),

    active = list(
        model_name = function() private$.model_name,
        model_path = function() private$.model_path,
        model_object = function() private$.model_object
    )
)

# Public Methods ---------------------------------------------------------------
.set_historical_data <- function(private, historical_data){
    private$.historical_data <- historical_data
    return(invisible())
}

.set_new_data <- function(private, new_data){
    private$.new_data <- new_data
    return(invisible())
}

.set_model <- function(private, model_object){
    private$.model_object <- model_object
    return(invisible())
}

# Private Methods --------------------------------------------------------------
.model_init <- function(private){
    base::get("model_init", envir = private$.env)()
    return(invisible())
}

.model_fit <- function(private){
    private$.model_object <- base::get("model_fit", envir = private$.env)(historical_data = private$.historical_data)
    return(invisible())
}



# High-Level Helper-Functions --------------------------------------------------
.load_model_components <- function(object){
    .assert_all_components_files_exist(object)
    .remove_model_components_from_env(object)
    sapply(object$.component_paths, source, local = object$.env)
    .assert_all_components_are_in_env(object)
}

# Low-Level Helper-Functions ---------------------------------------------------
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
