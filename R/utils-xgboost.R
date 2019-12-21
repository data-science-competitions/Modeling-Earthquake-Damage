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
    label <- eval(parse(text = 'xgboost::getinfo(dtrain, "label")'))
    x <- as.numeric(preds - label)
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
    y_hat <- preds
    y <- eval(parse(text = 'xgboost::getinfo(dtrain, "label")'))
    w <- eval(parse(text = 'xgboost::getinfo(dtrain, "weight")'))
    x <- as.numeric(y_hat - y)
    grad <- tanh(w * x)
    hess <- 1 / cosh(w * x)^2
    return(list(metric = "mae", grad = grad, hess = hess))
}

#' @title Micro Averaged F1 Score Evaluation Function
#' @description Converts numeric to class
#' @inheritParams fair_obj
#' @export
#' @keywords internal
#' @family xgboost functions
feval_f1 <- function(preds, dtrain){
    label <- eval(parse(text = 'xgboost::getinfo(dtrain, "label")'))
    y <- as_earthquake_damage(label)
    y_hat <- as_earthquake_damage(preds)
    f1 <- yardstick::f_meas_vec(truth = y, estimate = y_hat, estimator = "micro")
    return(list(metric = 'F1', value = f1))
}

#' @title Micro Averaged F1 score Evaluation Function
#' @inheritParams fair_obj
#' @export
#' @keywords internal
#' @family xgboost functions
feval_f1_micro <- function(preds, dtrain){
    y <- eval(parse(text = 'xgboost::getinfo(dtrain, "label")'))
    y_hat <- preds

    num_class <- y %>% unique() %>% length()
    y <- ordered(y, levels = 0:(num_class-1), labels = 1:num_class)
    y_hat <- ordered(y_hat, levels = 0:(num_class-1), labels = 1:num_class)

    f1 <- yardstick::f_meas_vec(truth = y, estimate = y_hat, estimator = "micro")
    return(list(metric = 'F1', value = f1))
}

#nocov end
