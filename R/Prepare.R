#' @title Data Preparation Interface for Non-Real-Time Analytic Applications
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
Prepare <- R6::R6Class(
    classname = "Prepare",
    cloneable = FALSE,
    lock_objects = FALSE,
    public = list(
        # Public Variables -----------------------------------------------------

        # Public Methods -------------------------------------------------------
        initialize = function()
        {
            message("Instantiating ", private$.ingest)
            private$.ingest <- base::get(private$.ingest)$new()

            message("Preparing Data")
            private$import_data.frames_from_Ingest()
            private$cast_data()
            private$clean_data()
            private$enrich_data()
        }),

    private = list(
        # Private Variables ----------------------------------------------------
        .ingest = getOption("dataflows.ingest.abstract", "Ingest"),

        # Private Methods ------------------------------------------------------
        import_data.frames_from_Ingest = function() .import_data.frames_from_Ingest(private),
        cast_data = function() invisible(private),
        clean_data = function() invisible(private),
        enrich_data = function() invisible(private)
    ),

    active = list(
        historical_data = function() private$.historical_data,
        new_data = function() private$.new_data,
        submission_format = function() private$.submission_format
    )
)#end Prepare

# Helper Functions -------------------------------------------------------------
#' @title Import Data Frames from Ingest to Prepare
#' @section Operations:
#' 1. Extract the data frames from ingest;
#' 2. Add unique identifier (UID) for each row; and
#' 3. Standardise column names.
#' @noRd
.import_data.frames_from_Ingest <- function(private){
    .standardise_col_names <- function(.data){
        colnames(.data) <- standardise_strings(colnames(.data), "lowercase", "underscores")
        return(.data)
    }

    for(element in names(private$.ingest)){
        if(is.data.frame(private$.ingest[[element]])){
            private[[paste0(".", element)]] <-
                private$.ingest[[element]] %>%
                .standardise_col_names()
        }
    }

    invisible(private)
}
