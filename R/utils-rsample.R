# get_rsample_training_set -----------------------------------------------------
#
#' @title Extract the Training Set from an rsample Object
#'
#' @description Given an rsample object and a split number, the function returns
#'   the training set as a data.frame.
#'
#' @inheritParams get_rsample_num_of_splits
#' @param split (`integer`) A number specifying what split has the required
#'   data.
#'
#' @return ('data.frame') The training set for the specified split.
#'
#' @export
#'
#' @family rsample utility functions
#'
get_rsample_training_set <- function(rset_obj, split) {
    ###########################
    ## Defensive Programming ##
    ###########################
    stopifnot(
        missing(rset_obj) == FALSE,
        missing(split) == FALSE,
        .is_positive_and_finite(split),
        .is_whole_number(split)
    )

    ###############################
    ## Find the Number of Splits ##
    ###############################
    K <- get_rsample_num_of_splits(rset_obj)
    stopifnot(.are_all_less_than_or_equal_to(split, K))

    ##############################
    ## Extract the Training Set ##
    ##############################
    if (K == 1) X <- rsample::training(rset_obj)
    if (K > 1) X <- rsample::training(rset_obj$splits[[split]])

    ############
    ## Return ##
    ############
    return(X)
}

# get_rsample_test_set ---------------------------------------------------------
#
#' @title Extract the Test Set from an rsample Object
#'
#' @description Given an rsample object and a split number, the function returns
#'   the test set as a data.frame.
#'
#' @inheritParams get_rsample_training_set
#'
#' @return ('data.frame') The test set for the specified split.
#'
#' @export
#'
#' @family rsample utility functions
#'
get_rsample_test_set <- function(rset_obj, split) {
    ###########################
    ## Defensive Programming ##
    ###########################
    stopifnot(
        isFALSE(missing(rset_obj)),
        isFALSE(missing(split)),
        .is_positive_and_finite(split),
        .is_whole_number(split)
    )

    ###############################
    ## Find the Number of Splits ##
    ###############################
    K <- get_rsample_num_of_splits(rset_obj)
    stopifnot(.are_all_less_than_or_equal_to(split, K))

    ##########################
    ## Extract the Test Set ##
    ##########################
    if (K == 1) X <- rsample::testing(rset_obj)
    if (K > 1) X <- rsample::testing(rset_obj$splits[[split]])

    ############
    ## Return ##
    ############
    return(X)
}

# get_rsample_num_of_splits ----------------------------------------------------
#
#' @title Find out How Many Splits an rsample Object Has
#'
#' @description Given an rsample object, the function returns the number of
#'   splits within the object.
#'
#' @details Common rsample options include:
#'  \describe{
#'   \item{\code{\link[rsample]{initial_split}}}{Returns 1}
#'   \item{\code{\link[rsample]{vfold_cv}}}{Returns \code{v * repeats}; i.e. if
#'   num of folds = 10, without repetitions, the return value is 10}
#'   \item{\code{\link[rsample]{bootstraps}}}{Returns \code{times}; i.e. if num
#'   of bootstrap samples = 25, the return value is 25}
#' }
#'
#' @param rset_obj An object created by the rsample package. See the details
#'   section under \code{get_rsample_num_of_splits}.
#'
#' @return (`integer`) Indicating how many splits are within the object.
#'
#' @export
#'
#' @family rsample utility functions
#'
get_rsample_num_of_splits <- function(rset_obj) {
    ###########################
    ## Defensive Programming ##
    ###########################
    stopifnot(
        missing(rset_obj) == FALSE,
        any(class(rset_obj) %in% c("rset", "rsplit"))
    )

    ###############################
    ## Find the Number of Splits ##
    ###############################
    K <- length(rset_obj$splits)
    K <- ifelse(K == 0, 1, K)

    ############
    ## Return ##
    ############
    return(K)
}
