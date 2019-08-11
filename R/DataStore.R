#' @title Storage Place for Hosting Application Datasets
#'
#' @description A DSO (DataStore Object) is known as the storage place to keep
#'   cleansed and consolidated transaction or master data at the lowest
#'   granularity level.
#'
#' @return (`DataStore`)
#'
#' @export
#'
#' @family Data Pipeline
#'
#' @section Further Reading:
#' * \href{https://en.wikipedia.org/wiki/Object_storage}{What is a DataStore Object?}
#'
#' @examples
#' \dontrun{
#' ds <- DataStore()
#' names(ds)
#' }
#'
#' @docType class
#' @format \code{\link[R6]{R6Class}} object.
#' @keywords data
DataStore <- R6::R6Class(
    classname = "DataStore",
    cloneable = FALSE,
    portable = FALSE,
    lock_objects = TRUE,
    public = list(
        # Public Variables -----------------------------------------------------
        initialize = function(){
            private$.data_model <- private$generate_data_model()
        }
        # Public Methods -------------------------------------------------------
    ),
    private = list(
        # Private Variables ----------------------------------------------------
        .data_model = NULL,
        # Private Methods ------------------------------------------------------
        generate_data_model = function() .generate_data_model()
    ),
    active = list(
        data_model = function() private$.data_model
    )
)#end DataStore

# Private Methods --------------------------------------------------------------
.generate_data_model <- function(){
    dm_object <- dplyr::src_df(env = PrepareData$new()) %>% dm::dm()
    dm_object <-
        dm_object %>%
        dm::cdm_add_pk(table = "historical_data", column = "building_id") %>%
        dm::cdm_add_pk(table = "new_data", column = "building_id") %>%
        dm::cdm_add_fk(table = "submission_format", column = "building_id", "new_data")
}

