# nocov start
# visualise_ccc -----------------------------------------------------------
#
#' @title Plot the Ground Truth vs the Predicted Values
#'
#' @param data (`data.frame`) A table containing the \code{truth} and\code{estimate} columns.
#' @param truth (`character`) The column identifier for the true results.
#' @param estimate (`character`) The column identifier for the predicted results.
#'
#' @seealso \url{https://www.statisticshowto.datasciencecentral.com/concordance-correlation-coefficient/}
#'
#' @family visualisation utility functions
#' @export
#'
visualise_ccc <- function(data, truth, estimate){
    graphics::par(pty = "s")
    graphics::plot(data[, truth], data[, estimate])
    graphics::title(
        main = "Ground Truth vs Predicted Values",
        xlab = "Ground Truth", ylab = "Predicted Values"
    )
    graphics::abline(a = 0, b = 1)
}
# nocov end
