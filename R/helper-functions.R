#nocov start
# Predicates --------------------------------------------------------------
.all_are_non_missing_nor_empty_strings <- function(x){
    return(identical(all(is.character(x)) & all(nchar(x) > 0), TRUE))
}

.is_a_non_missing_nor_empty_string <- function(x){
    return(identical(length(x) == 1 & is.character(x) & nchar(x) > 0, TRUE))
}

.are_disjoint_sets <- function(x, y){
    return(identical(length(intersect(x, y)), 0L))
}

.is_positive_and_finite <- function(x){
    return(identical(length(x) == 1 & is.numeric(x) & is.finite(x) & x > 0, TRUE))
}

# Assertions --------------------------------------------------------------
.assert_is_a_non_missing_nor_empty_string <- function(x){
    stopifnot(.is_a_non_missing_nor_empty_string(x))
    return(invisible())
}

.assert_is_environment <- function(x){
    stopifnot(is.environment(x))
    return(invisible())
}

.assert_is_positive_and_finite <- function(x){
    stopifnot(.is_positive_and_finite(x))
    return(invisible())
}
#nocov end
