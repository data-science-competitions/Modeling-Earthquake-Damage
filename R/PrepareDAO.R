#' @title Instantiate the Prepare Interface
#'
#' @description Data preprocessing is a data mining technique that involves
#'   transforming raw data into an understandable format
#'
#' @return (`Prepare`) An interface that defines an abstract type that contains
#'   no data but defines behaviours as method signatures.
#'
#' @export
#'
#' @family Data Pipeline
#'
#' @docType class
#' @format \code{\link[R6]{R6Class}} object.
#' @keywords data
PrepareDAO <- R6::R6Class(
    classname = "PrepareDAO",
    inherit = Prepare,
    cloneable = FALSE,
    lock_objects = FALSE,
    private = list(
        # Private Variables ----------------------------------------------------
        .ingest = getOption("dataflows.ingest.concrete", "IngestDAO"),

        # Private Methods ------------------------------------------------------
        import_data.frames_from_Ingest = function() .import_data.frames_from_Ingest(private),
        cast_data = function() .cast_data(private),
        clean_data = function() .clean_data(private),
        enrich_data = function() .enrich_data(private)
    ),

    active = list(
        historical_data = function() private$.historical_data,
        new_data = function() private$.new_data,
        submission_sample = function() private$.submission_sample
    )
)#end PrepareDAO

# Private Methods: High-level Functions ----------------------------------------
.cast_data <- function(private){
    message("-> Casting Data")
    invisible(private)
}

.clean_data <- function(private){
    message("-> Cleaning Data")
    invisible(private)
}

.enrich_data <- function(private){
    message("-> Enriching Data")
    invisible(private)
}
