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
    cloneable = FALSE,
    public = list(
        ## Public Methods
        initialize = function(path)
        {
            .set_shared_object("model_path", path, private$shared_env)
            .set_shared_object("model_name", basename(path), private$shared_env)

            .set_shared_object("link_function", function(x) x, private$shared_env)
            # .set_shared_object("predict_function", function(model, new_data) predict(model, new_data), private$shared_env)
            #
            private$.component_paths <- file.path(private$shared_env$model_path, paste0(private$.component_names,".R"))

            .load_model_components(private)
        },
        model_init = function() .model_init(private),
        model_fit = function() .model_fit(private),
        model_predict = function() .model_predict(private),
        model_store = function() .model_store(private),
        model_end = function() base::get("model_end", envir = private$shared_env)(),
        set_link_function = function(value) .set_shared_object("link_function", value, private$shared_env),
        set_historical_data = function(value) .set_shared_object("historical_data", value, private$shared_env),
        set_new_data = function(value) .set_shared_object("new_data", value, private$shared_env),
        set_model = function(value) .set_shared_object("model_object", value, private$shared_env),
        set_role_pk = function(value) .update_formula_variables(private, "role_pk", value),
        set_role_none = function(value) .update_formula_variables(private, "role_none", value),
        set_role_input = function(value) .update_formula_variables(private, "role_input", value),
        set_role_target = function(value) .update_formula_variables(private, "role_target", value)
    ),

    private = list(
        shared_env = new.env(),
        .component_names = c("model_init", "model_fit", "model_predict", "model_store", "model_end"),
        .component_paths = character(0)
    ),

    active = list(
        model_environment = function() private$shared_env,
        model_name = function() private$shared_env$model_name,
        model_path = function() private$shared_env$model_path,
        model_object = function() private$shared_env$model_object,
        model_formula = function() private$shared_env$model_formula,
        link_function = function() .get_shared_object("link_function", private$shared_env),
        response = function() .get_shared_object("response", private$shared_env)
    )
)

# Public Methods ---------------------------------------------------------------
.set_private_variable <- function(private, key, value){
    private[[key]] <- value
    return(invisible())
}

.update_formula_variables <- function(private, key, value){
    roles <- c("role_pk", "role_none", "role_input", "role_target")
    other_roles <- setdiff(roles, key)

    for(other_role in other_roles){
        this_role_value <- value
        other_role_value <- .get_shared_object(other_role, private$shared_env)
        intersecting_values <- intersect(this_role_value, other_role_value)
        if(is.null(intersecting_values)) next()
        if(length(intersecting_values) == 0) next()
        stop("\n", paste0(intersecting_values, collapse = ", "), " already in ", other_role)
    }

    .set_shared_object(key, value, private$shared_env)

    try(
        private$shared_env$model_formula <- .compose_formula(
            role_pk = .get_shared_object("role_pk", private$shared_env),
            role_none = .get_shared_object("role_none", private$shared_env),
            role_input = .get_shared_object("role_input", private$shared_env),
            role_target = .get_shared_object("role_target", private$shared_env)
        ), silent = TRUE)

    return(invisible())
}

# Private Methods --------------------------------------------------------------
.model_init <- function(private){
    base::get("model_init", envir = private$shared_env)()

    # Get all the objects in the current environment excluding the private
    # environment and assign them to the model environment
    for(n in setdiff(ls(environment(), all.names = TRUE), "private"))
        assign(n, get(n, environment()), private$shared_env)
    return(invisible())
}

.model_fit <- function(private){
    .check_model_fit_input_arguments(private)

    model_fit <- base::get("model_fit", envir = private$shared_env)

    private$shared_env$model_object <- model_fit(
        historical_data = private$shared_env$historical_data,
        model_formula = private$shared_env$model_formula
    )

    # Get all the objects in the current environment excluding the private
    # environment and assign them to the model environment
    for(n in setdiff(ls(environment(), all.names = TRUE), "private"))
        assign(n, get(n, environment()), private$shared_env)
    return(invisible())
}

.model_predict <- function(private){
    .check_model_predict_input_arguments(private)
    .add_rowid_to_new_data(private)

    model_predict <- base::get("model_predict", envir = private$shared_env)
    private$shared_env$response <- model_predict(
        new_data = private$shared_env$new_data,
        model_object = private$shared_env$model_object
    )

    .check_model_predict_output_arguments(private)
    .pack_model_predict_output_arguments(private)

    # Get all the objects in the current environment excluding the private
    # environment and assign them to the model environment
    for(n in setdiff(ls(environment(), all.names = TRUE), "private"))
        assign(n, get(n, environment()), private$shared_env)
    return(invisible())
}

.model_store <- function(private){
    base::get("model_store", envir = private$shared_env)()

    # Get all the objects in the current environment excluding the private
    # environment and assign them to the model environment
    for(n in setdiff(ls(environment(), all.names = TRUE), "private"))
        assign(n, get(n, environment()), private$shared_env)
    return(invisible())
}

# checks ------------------------------------------------------------------
.check_model_fit_input_arguments <- function(private){
    if(is.null(private$shared_env$historical_data))
        stop("\nhistorical_data is unset;\nDid you forget to use PentaModelObj$set_historical_data(<data-frame>)?")

    if(is.null(private$shared_env$role_input))
        stop("\nInput variables are unset;\nDid you forget to use PentaModelObj$set_role_input(<var-names>)?")

    if(is.null(private$shared_env$role_target))
        stop("\nTarget variable is unset;\nDid you forget to use PentaModelObj$set_role_target(<var-name>)?")

    if(length(private$shared_env$role_target) > 1)
        stop("\nMore than one target variable are set")

    .assert_columns_are_in_table(private$shared_env$historical_data, private$shared_env$role_input)
    .assert_columns_are_in_table(private$shared_env$historical_data, private$shared_env$role_target)
}

.check_model_predict_input_arguments <- function(private){
    if(is.null(private$shared_env$new_data))
        stop("\nnew_data is an empty data frame.\nDid you forget to use PentaModelObj$set_new_data(.data)?")

    if(is.null(private$shared_env$model_object))
        stop("\nmodel_object is an empty model.\nEither train a model with PentaModelObj$model_predict() OR preset a model with PentaModelObj$set_model(model_object)")

    .assert_columns_are_in_table(private$shared_env$new_data, private$shared_env$role_input)
    .assert_columns_are_in_table(private$shared_env$new_data, private$shared_env$role_target)
}

.check_model_predict_output_arguments <- function(private){
    if(any(is.na(private$shared_env$response)))
        stop("model_predict produced NA values.\nSee PentaModelObj$response")
    if(.nrecord(private$shared_env$response) != .nrecord(private$shared_env$new_data))
        stop("model_predict produced less/more values than in new_data.\nSee PentaModelObj$response")
}

# High-Level Helper-Functions --------------------------------------------------
.load_model_components <- function(object){
    .assert_all_components_files_exist(object)
    .remove_model_components_from_env(object)
    sapply(object$.component_paths, source, local = object$shared_env)
    .assert_all_components_are_in_env(object)
}

.pack_model_predict_output_arguments <- function(private){
    y_id <- private$shared_env$new_data[, private$shared_env$role_pk]
    response <- private$shared_env$response
    private$shared_env$response <-
        tibble::tibble(response) %>%
        tibble::add_column(rowid = y_id, .before = 0) %>%
        dplyr::rename_at("rowid", function(.) private$shared_env$role_pk) %>%
        dplyr::rename_all(function(colname) gsub(".*\\$", "", colname)) %>%
        as.data.frame(stringsAsFactors = FALSE)

    invisible(private)
}

.add_rowid_to_new_data <- function(private){
    if(is.null(private$shared_env$role_pk)){
        private$shared_env$new_data <-
            private$shared_env$new_data %>%
            tibble::rownames_to_column("rowid")
        .update_formula_variables(private, "role_pk", "rowid")
    }

    invisible(private)
}

# Low-Level Helper-Functions ---------------------------------------------------
.remove_model_components_from_env <- function(object){
    suppressWarnings(rm(list = object$.component_names, envir = object$shared_env))
}

.nrecord <- function(x) {
    if(isTRUE("data.frame" %in% class(x))){
        return(nrow(x))
    } else if (isTRUE("matrix" %in% class(x))) {
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

#nocov start
# Assertions --------------------------------------------------------------
.assert_all_components_files_exist <- function(object){
    for(component_path in object$.component_paths)
        if(isFALSE(file.exists(component_path)))
            stop(component_path, " doesn't exist")
}

.assert_all_components_are_in_env <- function(object){
    function_names_in_env <- utils::lsf.str(envir = object$shared_env)
    for(component_name in object$.component_names)
        if(isFALSE(component_name %in% function_names_in_env))
            stop(component_name, " doesn't exist")
}

.assert_columns_are_in_table <- function(.data, col_names){
    if(.is_subset(col_names, colnames(.data))) return(invisible())
    table_name <- deparse(substitute(.data))
    table_name <- gsub("^.*\\$", "", table_name)
    stop("\n", table_name, " doesn't contain the following columns: ", paste0(setdiff(col_names, colnames(.data)), collapse = ", "))
}

# Predicates --------------------------------------------------------------
.are_disjoint_sets <- function(x, y){
    if(is.null(x) | is.null(y)) return(FALSE)
    return(length(intersect(x, y)) == 0)
}

.is_subset <- function(x, y){
    if(is.null(x) | is.null(y)) return(FALSE)
    return(length(setdiff(x, y)) == 0)
}

.is_not_null <- function(x) isFALSE(is.null(x))

# CRUD API for Shared Environment -----------------------------------------
.set_shared_object <- function(key, value, envir){
    stopifnot(is.character(key), length(key) == 1)
    stopifnot(is.environment(envir))
    assign(x = key, value = value, envir = envir)
    invisible()
}

.get_shared_object <- function(key, envir){
    stopifnot(is.character(key), length(key) == 1)
    stopifnot(is.environment(envir))
    tryCatch(get(x = key, envir = envir), error = function(e) invisible())
}
#nocov end
