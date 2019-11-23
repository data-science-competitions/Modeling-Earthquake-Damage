#nocov start
#' @title Fair Loss Function
#'
#' @description
#' This function computes for xgboost's obj function the Mean Absolute Error
#' loss (MAE) gradient and hessian per value provided \code{preds} and
#' \code{dtrain}.
#'
#' @param preds The predictions
#' @param dtrain The xgboost model
#'
#' @return The gradient and the hessian of the Absolute Error per value in a
#'   list.
#'
#' @export
#' @keywords internal
#' @family xgboost functions
#'
fair_obj <- function(preds, dtrain){
    x <- as.numeric(preds - xgboost::getinfo(dtrain, "label"))
    c <- 1
    den <- abs(x)
    grad <- c * x / den
    hess <- c * c / den^2
    return(list(metric = "mae", grad = grad, hess = hess))
}

#' @title Log-Cosh Loss Function
#' @inheritParams fair_obj
#' @inherit fair_obj description return
#' @export
#' @keywords internal
#' @family xgboost functions
log_cosh_obj <- function(preds, dtrain){
    x <- as.numeric(preds - xgboost::getinfo(dtrain, "label"))
    grad <- tanh(x)
    hess <- 1 / cosh(x)^2
    return(list(metric = "mae", grad = grad, hess = hess))
}

#' @title Accuracy Evaluation Function
#' @inheritParams fair_obj
#' @export
#' @keywords internal
#' @family xgboost functions
feval_accuracy <- function(preds, dtrain){
    y <- as_earthquake_damage(xgboost::getinfo(dtrain, "label"))
    y_hat <- as_earthquake_damage(preds)
    accuracy <- yardstick::accuracy_vec(truth = y, estimate = y_hat)
    return(list(metric = 'accuracy', value = accuracy))
}

#nocov end
