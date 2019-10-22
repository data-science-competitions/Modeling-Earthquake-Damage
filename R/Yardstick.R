# Yardstick -------------------------------------------------------------------
#
#' @title Methods for Measuring Model Performance
#'
#' @description NA
#'
#' @details
#' 1. rmse
#' 2. xxx
#'
#' @param data (`data.frame`) A table containing the \code{truth} and\code{estimate} columns.
#' @param truth (`character`) The column identifier for the true results.
#' @param estimate (`character`) The column identifier for the predicted results.
#'
#' @return (`Yardstick`) A PentaModel API.
#' @export
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
Yardstick <- R6::R6Class(
    classname = "Yardstick",
    cloneable = FALSE,
    lock_objects = FALSE,
    public = list(
        ## Public Methods
        initialize = function(data, truth, estimate)
        {
            is.a.character <- function(x) identical(is.character(x) & length(x) == 1, TRUE)
            stopifnot(is.data.frame(data), is.a.character(truth), is.a.character(estimate))

            private$.data <- data
            private$.truth <- truth
            private$.estimate <- estimate
        }),
    private = list(
        ## Private Variables
        .data = data.frame(stringsAsFactors = FALSE),
        .truth = character(0),
        .estimate = character(0),
        ## Private Methods
        call_metric = function(metric) .call_metric(private, metric)
    ),
    active = list(
        rmse = function() private$call_metric(metric = "rmse"),
        mae = function() private$call_metric(metric = "mae"),
        rsq = function() private$call_metric(metric = "rsq"),
        ccc = function() private$call_metric(metric = "ccc")
    )
)

# Private Methods ---------------------------------------------------------
.call_metric <- function(private, metric){
    data <- private$.data
    truth <- private$.truth
    estimate <- private$.estimate

    command <- paste0("yardstick::", metric, "(data, !!truth, !!estimate)")
    eval(expr = parse(text = command))
}

