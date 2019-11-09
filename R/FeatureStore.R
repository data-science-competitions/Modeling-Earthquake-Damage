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
  public = list(),
  private = list(),
  active = list()
)#end DataStore
