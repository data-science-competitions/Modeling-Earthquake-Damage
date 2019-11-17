# ExplainerYardstick ------------------------------------------------------
#
#' @title Instantiate the Yardstick Class by Passing an explainer Object
#'
#' @param explainer (`explainer`) An object created by \link[DALEX]{explain}.
#'
#' @return (`Yardstick`) An object created by \link{Yardstick}.
#'
#' @export
#'
ExplainerYardstick <- function(explainer){
    stopifnot("explainer" %in% class(explainer))

    suppressWarnings({
        y <- tibble::as_tibble(explainer$y)[[1]]
        y_hat <- tibble::as_tibble(explainer$y_hat)[[1]]
    })
    data <- dplyr::bind_cols(truth = y, estimate = y_hat)

    Yardstick$
        new(data, "truth", "estimate")$
        insert_label(".label", explainer$label)$
        insert_label(".model", explainer$class)$
        insert_label(".p", ncol(explainer$data))
}
