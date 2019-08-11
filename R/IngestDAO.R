#' @title Instantiate the Ingest Interface
#'
#' @field path (`character`) A path to a folder where the raw data files
#'   are/will-be stored.
#'
#' @return (`Ingest`) An implementing of the `Ingest` interface.
#'
#' @export
#'
#' @family Data Pipeline
#'
#' @seealso \link{Ingest}
#'
#' @section Further Reading:
#' * \href{https://docs.microsoft.com/en-us/azure/data-explorer/ingest-data-overview}{What is data ingestion?}
#' * \href{https://en.wikipedia.org/wiki/Data_access_object}{What is data access object?}
#'
#' @examples
#' \dontrun{
#' db <- IngestDAO(path = getOption("path_dropzone", default = tempdir())
#' names(db)
#' }
#'
#' @docType class
#' @format \code{\link[R6]{R6Class}} object.
#' @keywords data
IngestDAO <- R6::R6Class(
    classname = "IngestDAO",
    inherit = Ingest,
    private = list(
        # Private Variables ----------------------------------------------------
        .path = character(0),
        .historical_data = tibble::tibble(),
        .new_data = tibble::tibble(),
        .submission_sample = tibble::tibble(),

        # Private Methods ------------------------------------------------------
        pull_data = function() .pull_data(private),
        import_data = function() .import_data(private)
    ),

    active = list(
        historical_data = function() private$.historical_data,
        new_data = function() private$.new_data,
        submission_sample = function() private$.submission_sample
    )
)#end IngestDAO

# Private Methods: High-level Functions ----------------------------------------
.pull_data <- function(private){
    path <- private$.path
    dir.create(path, showWarnings = FALSE, recursive = TRUE)

    sources <- c()
    targets <- c()

    for(k in seq_along(sources)){
        .download_missing_files(sources[[k]], targets[[k]])
    }

    invisible(private)
}

.import_data <- function(private){
    get("data")("mtcars", package = "datasets", envir = environment())

    private$.historical_data <- get("mtcars")[1:22,]
    private$.new_data <- get("mtcars")[23:32,]
    private$.submission_sample <- tibble::tibble(UID = rownames(private$.new_data))

    invisible(private)
}

# Private Methods: Low-level Functions -----------------------------------------
.download_missing_files <- function(source, target){
    if(identical(file.exists(target), FALSE))
        utils::download.file(source, target)
}
