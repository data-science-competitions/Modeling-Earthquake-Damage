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
        model_predict = function() .model_predict(private),
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
        .response = NULL,
        .env = environment(),
        .historical_data = NULL,
        .new_data = NULL
    ),

    active = list(
        model_name = function() private$.model_name,
        model_path = function() private$.model_path,
        model_object = function() private$.model_object,
        response = function() private$.response
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
    if(is.null(private$.historical_data))
        stop("\nhistorical_data is an empty data frame.\nDid you forget to use PentaModelObj$set_historical_data(.data)?")

    private$.model_object <- base::get("model_fit", envir = private$.env)(historical_data = private$.historical_data)

    return(invisible())
}

.model_predict <- function(private){
    if(is.null(private$.new_data))
        stop("\nnew_data is an empty data frame.\nDid you forget to use PentaModelObj$set_new_data(.data)?")
    if(is.null(private$.model_object))
        stop("\nmodel_object is an empty model.\nEither train a model with PentaModelObj$model_predict() OR preset a model with PentaModelObj$set_model(model_object)")

    private$.response <- base::get("model_predict", envir = private$.env)(new_data = private$.new_data, model_object = private$.model_object)

    if(any(is.na(private$.response)))
        stop("model_predict produced NA values.\nSee PentaModelObj$response")
    if(.nrecord(private$.response) != .nrecord(private$.new_data))
        stop("model_predict produced less/more values than in new_data.\nSee PentaModelObj$response")

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

.nrecord <- function(x) {
    if(isTRUE(class(x) %in% "data.frame")){
        return(nrow(x))
    } else if (isTRUE(class(x) %in% "matrix")) {
        return(dim(x)[1])
    } else {
        return(length(x))
    }
}
