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
  set.seed(1313)

  historical_data <-
    private$ds$data_model$historical_data %>%
    dplyr::sample_n(dplyr::n()) %>%
    dplyr::mutate(.set_bucket = dplyr::ntile(1:dplyr::n(), 26))

  new_data <- private$ds$data_model$new_data

  dplyr::bind_rows(historical_data = historical_data, new_data = new_data, .id = ".set_source") %>%
    tibble::add_column(".set_role" = NA_character_, .after = 1) %>%
    dplyr::mutate(
      .set_role = dplyr::if_else(.set_bucket %in% 1:6, "train", .set_role),
      .set_role = dplyr::if_else(.set_bucket %in% 7:10, "test", .set_role),
      .set_role = dplyr::if_else(.set_bucket %in% 11:26, "calibration", .set_role)
    ) %>%
    dplyr::select(-.set_bucket)
}

#' @title Craft Geo Features
#' @description Treat high cardinality categorical variables.
#' @param private R6 private component
#' @details Fields
#' * **cat_P**: a “prevalence fact” about a categorical level. Tells us if the
#' original level was rare or common. Probably not good for direct use in a
#' model, but possibly useful for meta-analysis on the variable.
#' * **cat_N**: a single variable regression model of the difference in outcome
#' expectation conditioned on the observed value of the original variable. In
#' our example: x_catN = E[y|x] - E[y]. This encoding is especially useful for
#' categorical variables that have a large number of levels, but be aware it can
#' obscure degrees of freedom if not used properly.
#' * **cat_D**: a “deviation fact” about a categorical level tells us if 'y' is
#' concentrated or diffuse when conditioned on the observed level of the
#' original categorical variable. Probably not good for direct use in a model,
#' but possibly useful for meta-analysis on the variable.
#' @return (`data.frame`) A table with treated geo features
.craft_geo_features <- function(private){
  set.seed(1949)

  tidy_data <-
    .craft_tidy_data(private) %>%
    dplyr::select(dplyr::starts_with("."), building_id, dplyr::starts_with("geo_"), damage_grade)

  treat_plan <-
    vtreat::mkCrossFrameNExperiment(
      dframe = tidy_data %>% dplyr::filter(.set_role %in% "calibration"),
      varlist = c("geo_level_1_id", "geo_level_2_id", "geo_level_3_id"),
      outcome = "damage_grade",
      ncross = 2^3,
      verbose = getOption("verbose")
    )

  tidy_geo <-
    vtreat::prepare(treatmentplan = treat_plan$treatments, dframe = tidy_data) %>%
    tibble::add_column(building_id = tidy_data$building_id, .before = TRUE) %>%
    dplyr::select(
      building_id,
      dplyr::starts_with("geo_level_1_id_cat"),
      dplyr::starts_with("geo_level_2_id_cat"),
      dplyr::starts_with("geo_level_3_id_cat")
    )

  return(tidy_geo)
}

