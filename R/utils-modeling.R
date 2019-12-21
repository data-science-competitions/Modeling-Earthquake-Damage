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
    stopifnot(is.vector(estimate))
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

#' @title Merge YAML List Elements at 2nd Depth Level
#'
#' @param x (`list`) YAML list.
#'
#' @return (`list`) The YAML list with 1 Depth Level
#'
#' @export
#'
#' @examples
#' \dontrun{
#'  x <- list(a = list(2, 4, 6), b = 50)
#'  y <- list(a = c(2, 4, 6), b = 50)
#'  identical(lapply(x, merge_elements), y)
#' }
merge_elements <- function(x){
    if(!is.list(x)) return(x)
    elements <- vector()
    for(element in x) elements <- c(elements, element)
    return(elements)
}
#nocov end
