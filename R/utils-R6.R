# Shared Environment CRUD API ---------------------------------------------
utils::globalVariables("shared_env")

#' @title Create an Object in Shared Evnironment
#' @param key (`character`) The name of the object.
#' @param value (`?`) The object.
#' @section CRUD: \url{https://en.wikipedia.org/wiki/Create,_read,_update_and_delete}
#' @family Shared Environment CRUD API
#' @keywords internal
#' @export
#' @noRd
.create_shared_env <- function(key, value){
    stopifnot(is.character(key), length(key) == 1)
    stopifnot(exists("shared_env"), is.environment(shared_env))
    assign(key, value, envir = shared_env)
    invisible()
}

#' @title Read an Object from Shared Evnironment
#' @inheritParams .create_shared_env
#' @return The queried object.
#' @inheritSection .create_shared_env CRUD
#' @family Shared Environment CRUD API
#' @keywords internal
#' @export
#' @noRd
.read_shared_env <- function(key){
    stopifnot(is.character(key), length(key) == 1)
    stopifnot(exists("shared_env"), is.environment(shared_env))
    tryCatch(get(key, envir = shared_env), error = function(e) invisible())
}

#' @title Update Object Within Shared Evnironment
#' @inheritParams .create_shared_env
#' @inheritSection .create_shared_env CRUD
#' @family Shared Environment CRUD API
#' @keywords internal
#' @export
#' @noRd
.update_shared_env <- function(key, value){
    stopifnot(exists("shared_env"), is.environment(shared_env))
    .create_shared_env(key, value)
    invisible()
}

#' @title Delete an Object Within Shared Evnironment
#' @inheritParams .create_shared_env
#' @inheritSection .create_shared_env CRUD
#' @family Shared Environment CRUD API
#' @keywords internal
#' @export
#' @noRd
.delete_shared_env <- function(key){
    stopifnot(exists("shared_env"), is.environment(shared_env))
    rm(list = key, envir = shared_env)
    invisible()
}
