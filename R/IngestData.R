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
IngestData <- R6::R6Class(
    classname = "IngestData",
    inherit = Ingest,
    private = list(
        # Private Variables ----------------------------------------------------

        # Private Methods ------------------------------------------------------
        pull_data = function() .pull_data_from_GitHub(private),
        import_data = function() .import_data_from_CSV(private)
    ),

    active = list(
        historical_data = function() private$.historical_data,
        new_data = function() private$.new_data,
        submission_format = function() private$.submission_format
    )
)#end IngestDAO

# Private Methods: High-level Functions ----------------------------------------
.pull_data_from_GitHub <- function(private){
    path <- private$.path
    dir.create(path, showWarnings = FALSE, recursive = TRUE)

    project <- "mockdb/db-modeling.earthquake.damage"
    file_path <- "data.zip"
    target <- file.path(path, file_path)

    if(identical(file.exists(target), FALSE)){#nocov start
        message("--> Downloading ", basename(target))
        .download_file_from_GitHub(target, project, file_path)

        message("--> Extracting files from ", basename(target))
        .unzip_files(target, path)
    }#nocov end

    invisible(private)
}

.import_data_from_CSV <- function(private){
    path <- private$.path

    private$.new_data <- .read_csv(file.path(path, "test_values.csv"))
    private$.submission_format <- .read_csv(file.path(path, "submission_format.csv"))
    train_values <- .read_csv(file.path(path, "train_values.csv"))
    train_labels <- .read_csv(file.path(path, "train_labels.csv"))
    private$.historical_data <- dplyr::left_join(train_values, train_labels, by = "building_id")

    invisible(private)
}

# Private Methods: Low-level Functions -----------------------------------------
#' @title Download a File from GitHub
#' @param dest_file (`character`) The path where the downloaded file is saved.
#' @return NULL
#' @noRd
.download_file_from_GitHub <- function(dest_file, project, file_path, ref = "master"){
    `%+%` <- function(a,b) paste0(a,"/",b)

    file_url <-
        "https://raw.githubusercontent.com" %+%
        project %+%
        ref %+%
        file_path

    utils::download.file(file_url, dest_file)

    return(invisible())
}

.unzip_files <- function(zip_file, exdir){
    zip::unzip(zipfile = zip_file, overwrite = TRUE, exdir = exdir)
    return(invisible())
}

.read_csv <- function(file){
    suppressMessages(table <- readr::read_csv(file, progress = FALSE))
    table <- as.data.frame(table, stringsAsFactors = FALSE)
    invisible(table)
}
