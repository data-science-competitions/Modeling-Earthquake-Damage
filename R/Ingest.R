#' @title Data Ingestion Interface for Non-Real-Time Analytic Applications
#'
#' @description Data ingestion is the process used to load data records from one
#'   or more sources to create or update a table in R session. Once ingested,
#'   the data becomes available for query.
#'
#' @field path (`character`) A path to a folder where the raw data files
#'   are/will-be stored.
#'
#' @return (`Ingest`) An interface that defines an abstract type that contains
#'   no data but defines behaviours as method signatures.
#'
#' @export
#'
#' @family Data Pipeline
#'
#' @note The methods defined in an interface contain no code and thus cannot
#'   themselves be called; they must be implemented by non-abstract code to be
#'   run when they are invoked.
#'
#' @section Interface Methods:
#' Define \code{active binding function} as a function that looks like a
#' variable but actually invokes a function each time it is accessed.
#'
#' The interface specifies three \code{active binding functions}, each fetches a
#' data table.
#'
#' In addition, the interface specifies a
#' \href{https://en.wikipedia.org/wiki/Template_method_pattern}{template method
#' pattern} for pulling and importing the data.
#'
#' @section Further Reading:
#' * \href{https://docs.microsoft.com/en-us/azure/data-explorer/ingest-data-overview}{What is data ingestion?}
#' * \href{https://en.wikipedia.org/wiki/Template_method_pattern}{What is template method pattern?}
#'
#' @examples
#' \dontrun{
#' db <- Ingest(path = getOption("path_dropzone", default = tempdir())
#' names(db)
#' }
#'
#' @docType class
#' @format \code{\link[R6]{R6Class}} object.
#' @keywords data
Ingest <- R6::R6Class(
    classname = "Ingest",
    cloneable = FALSE,
    lock_objects = FALSE,
    public = list(
        # Public Variables -----------------------------------------------------

        # Public Methods -------------------------------------------------------
        initialize = function(path = getOption("path_dropzone", default = tempdir()))
        {
            message("Ingesting Data")
            private$.path <- path
            private$pull_data()
            private$import_data()
        }),

    private = list(
        # Private Variables ----------------------------------------------------
        .path = character(0),
        .historical_data = tibble::tibble(),
        .new_data = tibble::tibble(),
        .submission_format =  tibble::tibble(),

        # Private Methods ------------------------------------------------------
        pull_data = function() invisible(private),
        import_data = function() invisible(private)
    ),

    active = list(
        historical_data = function() private$.historical_data,
        new_data = function() private$.new_data,
        submission_format = function() private$.submission_format
    )
)#end Ingest
