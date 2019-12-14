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
    geo_features = function() .craft_geo_features(private),
    age_features = function() .craft_age_features(private),
    mfa_features = function() .craft_mfa_features(private)
  )
)#end DataStore

# Private Methods ---------------------------------------------------------
utils::globalVariables(c(".set_bucket", ".set_role", "age_NA"))

.craft_tidy_data <- function(private){
  set.seed(1313)

  # Get the Data
  historical_data <- private$ds$data_model$historical_data %>% tibble::add_column(.set_role = NA_character_, .after = 1)
  new_data <- private$ds$data_model$new_data %>% tibble::add_column(.set_role = "test", .after = 1)

  # Mark new_data geo_level_3_id in historical_data
  historical_data <-
    historical_data %>%
    dplyr::mutate(.set_role = ifelse(building_id %in% new_data$geo_level_3_id, "calibration", .set_role))

  # Sample observations into buckets
  historical_data <-
    historical_data %>%
    dplyr::sample_n(dplyr::n()) %>%
    dplyr::mutate(.set_bucket = dplyr::ntile(1:dplyr::n(), 26))

  # Allocate observations to sets
  dplyr::bind_rows(historical_data = historical_data, new_data = new_data, .id = ".set_source") %>%
    dplyr::mutate(
      .set_role = dplyr::if_else(is.na(.set_role) & .set_bucket %in% 01:06, "train", .set_role),
      .set_role = dplyr::if_else(is.na(.set_role) & .set_bucket %in% 07:10, "validation", .set_role),
      .set_role = dplyr::if_else(is.na(.set_role) & .set_bucket %in% 11:26, "calibration", .set_role)
    ) %>%
    dplyr::rename(
      land_surface_condition_fct = land_surface_condition,
      position_fct = position,
      plan_configuration_fct = plan_configuration,
      legal_ownership_status_fct = legal_ownership_status
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
#' @keywords internal
#' @noRd
.craft_geo_features <- function(private){
  set.seed(1949)

  tidy_data <-
    .craft_tidy_data(private) %>%
    dplyr::select(dplyr::starts_with("."), "building_id", dplyr::starts_with("geo_"), "damage_grade") %>%
    dplyr::mutate_if(is.factor, forcats::fct_lump_min, min = 5, other_level = "Rare")

  treat_plan <-
    vtreat::mkCrossFrameNExperiment(
      dframe = tidy_data %>% dplyr::filter(.set_source %in% "historical_data"),
      varlist = c("geo_level_1_id", "geo_level_2_id", "geo_level_3_id"),
      outcome = "damage_grade",
      rareCount = 0,
      ncross = 2^3,
      verbose = getOption("verbose"),
      parallelCluster = getOption("parallel.cluster"),
      use_parallel = getOption("parallel.enable", FALSE)
    )

  tidy_geo <-
    vtreat::prepare(treatmentplan = treat_plan$treatments, dframe = tidy_data) %>%
    tibble::add_column(building_id = tidy_data$building_id, .before = TRUE) %>%
    dplyr::select(
      building_id,
      dplyr::matches("^geo_level_[1-3]_id_cat"),
      dplyr::matches("^geo_level_[1-3]_id_lev")
    )

  return(tidy_geo)
}

.craft_age_features <- function(private){
  tidy_age <-
    .craft_tidy_data(private) %>%
    dplyr::select(building_id, age) %>%
    dplyr::mutate(age = ifelse(age > 150, NA, age)) %>%
    treat_non_finite(replace = list(age = 250)) %>%
    dplyr::rename(treat_age = age, treat_age_NA = age_NA)

  return(tidy_age)
}

.craft_mfa_features<- function(private){
  tidy_has <-
    .craft_tidy_data(private) %>%
    dplyr::select(
      building_id,
      dplyr::starts_with("has_"),
      dplyr::ends_with("_type"),
      dplyr::ends_with("_fct")
    ) %>%
    purrr::modify_if(is.logical, factor, levels = c("FALSE", "TRUE")) %>%
    column_to_rownames("building_id")

  MFA_object <-
    tidy_has %>%
    FactoMineR::MFA(
      group = c(11, 11, 4, 4),
      type = c("n", "n", "n", "n"),
      ncp = 10,
      name.group = c("superstructure", "secondary_use", "type", "misc"),
      graph = FALSE
    )

  MFA_scores <-
    tibble::as_tibble(MFA_object$ind$coord, rownames = "building_id") %>%
    dplyr::rename_at(
      dplyr::vars(dplyr::starts_with("Dim.")),
      function(x) paste0("mfa_dim_", stringr::str_remove(x, "Dim."))
    )

  return(MFA_scores)
}
