#nocov start

#' @title Map Earthquake Damage Continuous Measurements to Factor Levels
#'
#' @param estimate (`numeric`) A vector of numeric predictions.
#' @param damage_grades (`numeric`) A vector of possible damage grades.
#'
#' @return (`factor`) A factor vector with danage grades as levels.
#'
#' @export
#'
as_earthquake_damage <- function(estimate, damage_grades = 1:3){
    fun <- function(estimate, damage_grades) which.min(abs(estimate - damage_grades))
    labels <- sapply(estimate, fun, damage_grades = damage_grades)
    factor(labels, levels = seq_along(damage_grades), labels = damage_grades)
}

#' @title Find Column Names in Dataset via Regular Expression
#'
#' @param .data (`data.frame`) The dataset.
#' @param pattern (`character`) The regular expression.
#'
#' @return (`character`) The column names matching the regular expression
#'
#' @export
match_columns <- function(.data, pattern){
    tidyselect::vars_select(names(.data), dplyr::matches(pattern))
}

#nocov end
