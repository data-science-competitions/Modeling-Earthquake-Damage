# FeatureStore -----------------------------------------------------------------
#
#' @title A Data Management Layer for Machine Learning
#'
#' @description A feature store serves as a repository of features that can be
#'   used on the training and evaluation of machine learning models.
#'
#' @return (`FeatureStore`)
#'
#' @export
#'
#' @family Data Pipeline
#'
#' @section Further Reading:
#' * \href{https://towardsdatascience.com/introducing-feast-5eb4d539c13f}{What is a feature store?}
#'
#' @examples
#' \dontrun{
#' fs <- FeatureStore()
#' names(fs)
#' }
#'
#' @docType class
#' @format \code{\link[R6]{R6Class}} object.
#' @keywords data
FeatureStore <- R6::R6Class(
  classname = "FeatureStore",
  cloneable = FALSE,
  portable = FALSE,
  lock_objects = TRUE,
  public = list(
    initialize = function(){
      private$ds <- DataStore$new()
    }
  ),
  private = list(ds = NULL),
  active = list(
    tidy_data = function() .craft_tidy_data(private),
    geo_features = function() .craft_geo_features(private)
  )
)#end DataStore

# Private Methods ---------------------------------------------------------
.craft_tidy_data <- function(private){
  historical_data <- private$ds$data_model$historical_data
  new_data <- private$ds$data_model$new_data
  dplyr::bind_rows(historical_data = historical_data, new_data = new_data, .id = "source")
}

.craft_geo_features <- function(private){
  geo_features <-
    .craft_tidy_data(private) %>%
    dplyr::select(building_id, dplyr::starts_with("geo_"))
}

