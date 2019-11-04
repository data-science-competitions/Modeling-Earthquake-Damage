# Yardstick -------------------------------------------------------------------
#
#' @title Methods for Measuring Model Performance
#'
#' @name Yardstick
#'
#' @description Encapsulate `yardstick` functions in an `R6` object.
#'
#' @section Constructor Arguments:
#' * \code{data} (`data.frame`) A table containing the \code{truth} and
#' \code{estimate} columns.
#' * \code{truth} (`character`) The column identifier for the true results.
#' * \code{estimate} (`character`) The column identifier for the predicted
#' results.
#'
#' @section Public Methods:
#' * \code{set_threshold} TRUE if x > threshold; FALSE if x <= threshold.
#' * \code{set_transformation_function} A function that converts \code{estimate}
#' into a factor which is comparable to \code{truth}. That means, that factor
#' levels in the former must be included in the latter.
#' * \code{insert_label}
#' * \code{delete_label}
#' * \code{plot_gain_curve} A cumulative gains curve shows the total number of
#' events captured by a model over a given number of samples.
#' * \code{plot_lift_curve} A lift curve shows the ratio of a model to a random
#' guess ('model cumulative sum' / 'random guess').
#'
#' @return (`Yardstick`) A Yardstick object.
#'
#' @seealso \url{https://tidymodels.github.io/yardstick/}
#'
#' @examples
#' \dontrun{
#' mpg_hat <- mtcars$mpg
#' data <- cbind(mpg_hat, mtcars)
#'
#' metrics <- Yardstick$new(data = data, truth = mpg, estimate = mpg_hat)
#' metrics$rmse
#' }
#'
#' @import ggplot2
#' @import yardstick
#' @export
#'
Yardstick <- R6::R6Class(
    classname = "Yardstick",
    cloneable = FALSE,
    lock_objects = FALSE,
    public = list(
        ## Public Methods
        initialize = function(data, truth, estimate){
            is.a.character <- function(x) identical(is.character(x) & length(x) == 1, TRUE)
            stopifnot(is.data.frame(data), is.a.character(truth), is.a.character(estimate))

            private$.data <- data
            private$.truth <- truth
            private$.estimate <- estimate
        },
        set_threshold = function(value) .set_threshold(value, private),
        set_transformation = function(fun) .set_transformation(fun, private),
        insert_label = function(key, value) .insert_label(key, value, private),
        delete_label = function(key) .delete_label(key, private),
        plot_gain_curve = function() .plot_gain_curve(private),
        plot_lift_curve = function() .plot_lift_curve(private)
    ),
    private = list(
        ## Private Variables
        .threshold = NULL,
        .transformation_function = NULL,
        .dictionary = data.frame(key = c(".metric", ".estimator", ".estimate"), value = NA_character_, stringsAsFactors = FALSE),
        .data = data.frame(stringsAsFactors = FALSE),
        .truth = character(0),
        .estimate = character(0),
        ## Private Methods
        call_class_metric = function(metric) .call_class_metric(private, metric),
        call_numeric_metric = function(metric) .call_numeric_metric(private, metric),
        return = function() invisible(get("self", envir = parent.frame(2)))
    ),
    active = list(
        keys = function() private$.dictionary$key,
        # Class metrics (hard predictions)
        accuracy = function() private$call_class_metric(metric = "accuracy"),
        # Class probability metrics
        # Numeric metrics
        rmse = function() private$call_numeric_metric(metric = "rmse"),
        mae = function() private$call_numeric_metric(metric = "mae"),
        rsq = function() private$call_numeric_metric(metric = "rsq"),
        ccc = function() private$call_numeric_metric(metric = "ccc")
    )
)

# Public Methods ----------------------------------------------------------
.set_threshold <- function(value, private){
    private$.threshold <- value
    private$return()
}

.set_transformation <- function(fun, private){
    stopifnot(is.function(fun))
    private$.transformation_function <- fun
    private$return()
}

.insert_label <- function(key, value, private){
    new_entry <- data.frame(key = key, value = value, stringsAsFactors = FALSE)
    private$.dictionary <- rbind(new_entry, private$.dictionary) %>% dplyr::distinct(key, .keep_all = TRUE)
    private$return()
}

.delete_label <- function(key, private){
    dictionary <- private$.dictionary
    private$.dictionary <- dictionary[!dictionary$key %in% key, ]
    private$return()
}

.plot_gain_curve <- function(private){
    .check_classification_plot_prerequisites(private)

    ggplot_data <- .get_classification_plot_data(private)

    ggplot_fig <-
        ggplot_data %>%
        yardstick::gain_curve(!!private$.truth, !!private$.estimate) %>%
        autoplot() +
        coord_fixed(ratio = 1) +
        scale_x_continuous(breaks = seq(0, 100, by = 10), expand = c(0,0)) +
        scale_y_continuous(breaks = seq(0, 100, by = 10), expand = c(0,0))

    return(ggplot_fig)
}

.plot_lift_curve <- function(private){
    .check_classification_plot_prerequisites(private)

    ggplot_data <- .get_classification_plot_data(private)

    ggplot_fig <-
        ggplot_data %>%
        yardstick::lift_curve(!!private$.truth, !!private$.estimate) %>%
        autoplot() +
        scale_x_continuous(breaks = seq(0, 100, by = 10)) +
        scale_y_continuous(breaks = seq(1, 100, by = 0.5))

    return(ggplot_fig)
}

# Private Methods ---------------------------------------------------------
.call_numeric_metric <- function(private, metric){
    dictionary <- private$.dictionary
    data <- private$.data
    truth <- private$.truth
    estimate <- private$.estimate

    command <- paste0("yardstick::", metric, "(data, !!truth, !!estimate)")
    results <- eval(expr = parse(text = command))
    results <- results[, colnames(results) %in% dictionary$key]

    for(key in dictionary$key){
        if(key %in% colnames(results)) {
            next
        } else {
            value <- dictionary %>% dplyr::filter(key == !!key) %>% .$value
            results <- results %>% tibble::add_column(!!key := value, .before = 0)
        }# end if-else
    }# end for loop

    return(results)
}

.call_class_metric <- function(private, metric){
    is_not_factor <- function(x) !identical(is.factor(x), TRUE)
    combine_factors_levels <- function(.data, .truth, .estimate){
        .data %>%
            dplyr::mutate(
                !!.truth := forcats::fct_c(.data[[.truth]], .data[[.estimate]])[1:dplyr::n()],
                !!.estimate := forcats::fct_c(.data[[.truth]], .data[[.estimate]])[(dplyr::n()+1):(2*dplyr::n())]
            )
    }

    transformation_function <- private$.transformation_function
    dictionary <- private$.dictionary
    data <- private$.data
    truth <- private$.truth
    estimate <- private$.estimate

    if(is_not_factor(data[[estimate]])) data <- data %>% dplyr::mutate(!!estimate := transformation_function(estimate))
    if(is_not_factor(data[[truth]])) data <- data %>% dplyr::mutate(!!truth := transformation_function(truth))

    data <- combine_factors_levels(data, truth, estimate)

    command <- paste0("yardstick::", metric, "(data, !!truth, !!estimate)")
    results <- eval(expr = parse(text = command))
    results <- results[, colnames(results) %in% dictionary$key]

    for(key in dictionary$key){
        if(key %in% colnames(results)) {
            next
        } else {
            value <- dictionary %>% dplyr::filter(key == !!key) %>% .$value
            results <- results %>% tibble::add_column(!!key := value, .before = 0)
        }# end if-else
    }# end for loop

    return(results)
}

# Public Methods Helper Functions -----------------------------------------
.check_classification_plot_prerequisites <- function(private){
    data <- private$.data
    truth <- private$.truth
    estimate <- private$.estimate
    threshold <- private$.threshold

    if(data[[truth]] %>% is.numeric()){
        if(is.null(threshold)) stop("No threshold value is defined; use set_threshold() to define it.")
        if(max(data[[truth]]) > 1 | min(data[[truth]]) < 0) stop("`truth` has values outside the bounded interval [0,1]")
    }

    if(data[[estimate]] %>% is.numeric()){
        if(max(data[[estimate]]) > 1 | min(data[[estimate]]) < 0) stop("`estimate` has values outside the bounded interval [0,1]")
    }

    invisible()
}

.get_classification_plot_data <- function(private){
    data <- private$.data
    truth <- private$.truth
    estimate <- private$.estimate
    threshold <- private$.threshold

    ggplot_data <- dplyr::mutate(data, !!truth := factor(truth > threshold) %>% stats::relevel(ref = "TRUE"))

    return(ggplot_data)
}
