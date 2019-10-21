# evaluate_model ----------------------------------------------------------
#
#' @title  Estimate How Well a Model Works
#'
#' @description Calculate the model's \code{"rmse", "mae", "iic"}.
#'
#' @param data (`data.frame`) A table containing the \code{truth} and\code{estimate} columns.
#' @param truth (`character`) The column identifier for the true results.
#' @param estimate (`character`) The column identifier for the predicted results.
#' @param metrics (`character`) Metrics to use for the evaluation; see options at \url{https://tidymodels.github.io/yardstick/articles/metric-types.html}.
#'
#' @return (`data.frame`) A table with columns .metric, .estimator, and
#'   .estimate and 1 row of values. For grouped data frames, the number of rows
#'   returned will be the same as the number of groups.
#'
#' @family modeling utility functions
#' @export
#'
evaluate_model <- function(data, truth, estimate, metrics){
    stopifnot(all(c(truth, estimate) %in% colnames(data)))

    # Regression Metrics
    data[, truth] <- data[, truth] %>% as.character() %>% as.numeric()
    regression_metrics <- tibble::tibble()
    for(metric in metrics){
        command <- paste0("yardstick::", metric, "(data, !!truth, !!estimate)")
        regression_metric <- try(eval(expr = parse(text = command)), silent = TRUE)
        if("try-error" %in% class(regression_metric)) next()
        regression_metrics <- dplyr::bind_rows(regression_metrics, regression_metric)
    }

    # Classification Metrics
    data[, truth] <- data[, truth] %>% as.factor()
    classification_metrics <- tibble::tibble()

    # Return
    metrics <- dplyr::bind_rows(regression_metrics, classification_metrics)
    return(metrics %>% as.data.frame(stringsAsFactors = FALSE))
}

