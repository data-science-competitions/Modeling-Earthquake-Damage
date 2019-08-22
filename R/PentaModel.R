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
        set_historical_data = function(value) .set_private_variable(private, ".historical_data", value),
        set_new_data = function(value) .set_private_variable(private, ".new_data", value),
        set_model = function(value) .set_private_variable(private, ".model_object", value),
        set_role_pk = function(value) .update_formula_variables(private, ".role_pk", value),
        set_role_none = function(value) .update_formula_variables(private, ".role_none", value),
        set_role_input = function(value) .update_formula_variables(private, ".role_input", value),
        set_role_target = function(value) .update_formula_variables(private, ".role_target", value)
    ),

    private = list(
        .component_names = c("model_init", "model_fit", "model_predict", "model_store", "model_end"),
        .component_paths = character(0),
        .model_name = character(0),
        .model_path = character(0),
        .model_object = NULL,
        .model_formula = NULL,
        .response = NULL,
        .env = environment(),
        .historical_data = NULL,
        .new_data = NULL,
        .role_pk = NULL,
        .role_none = NULL,
        .role_input = NULL,
        .role_target = NULL
    ),

    active = list(
        model_name = function() private$.model_name,
        model_path = function() private$.model_path,
        model_object = function() private$.model_object,
        model_formula = function() private$.model_formula,
        response = function() private$.response
    )
)

# Public Methods ---------------------------------------------------------------
.set_private_variable <- function(private, key, value){
    private[[key]] <- value
    return(invisible())
}

# Private Methods --------------------------------------------------------------
.model_init <- function(private){
    base::get("model_init", envir = private$.env)()
    return(invisible())
}

.model_fit <- function(private){
    .check_model_fit_input_arguments(private)

    private$.model_object <-
        base::get("model_fit", envir = private$.env)(
            historical_data = private$.historical_data,
            model_formula = private$.model_formula
        )

    return(invisible())
}

.model_predict <- function(private){
    .check_model_predict_input_arguments(private)

    private$.response <- base::get("model_predict", envir = private$.env)(new_data = private$.new_data, model_object = private$.model_object)
    private$.response <- as.data.frame(private$.response, stringsAsFactors = FALSE)
    colnames(private$.response) <- gsub("^private\\$\\.","", colnames(private$.response))

    if(any(is.na(private$.response)))
        stop("model_predict produced NA values.\nSee PentaModelObj$response")
    if(.nrecord(private$.response) != .nrecord(private$.new_data))
        stop("model_predict produced less/more values than in new_data.\nSee PentaModelObj$response")

    return(invisible())
}

.update_formula_variables <- function(private, key, value){
    .set_private_variable(private, key, value)

    try(
        private$.model_formula <- .compose_formula(
            role_pk = private$.role_pk,
            role_none = private$.role_none,
            role_input = private$.role_input,
            role_target = private$.role_target
        ), silent = TRUE)

    return(invisible())
}

.check_model_fit_input_arguments <- function(private){
    if(is.null(private$.historical_data))
        stop("\nhistorical_data is unset;\nDid you forget to use PentaModelObj$set_historical_data(<data-frame>)?")

    # if(is.null(private$.role_pk))
    #     stop("Primary Key variable is unset;\nDid you forget to use PentaModelObj$set_role_pk(<var-name>)?")

    if(is.null(private$.role_input))
        stop("Input variables are unset;\nDid you forget to use PentaModelObj$set_role_input(<var-names>)?")

    if(is.null(private$.role_target)){
        stop("Target variable is unset;\nDid you forget to use PentaModelObj$set_role_target(<var-name>)?")
    } else if (!identical(length(private$.role_target), 1L)){
        stop("More than one target variable are set")
    }
}

.check_model_predict_input_arguments <- function(private){
    if(is.null(private$.new_data))
        stop("\nnew_data is an empty data frame.\nDid you forget to use PentaModelObj$set_new_data(.data)?")
    if(is.null(private$.model_object))
        stop("\nmodel_object is an empty model.\nEither train a model with PentaModelObj$model_predict() OR preset a model with PentaModelObj$set_model(model_object)")
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

.compose_formula <- function(role_pk = NULL, role_none = NULL, role_input, role_target){
    X <- role_input %>% setdiff(role_none) %>% setdiff(role_pk) %>% setdiff(role_target)
    y <- role_target

    stats::formula(paste(y, "~", paste(X, collapse = " + ")))
}
