#' @title Prepare everything the prediction model needs
model_init <- function(){
    # Helper Functions ---------------------------------------------------------
    calc_confusion_matrix <<- function(actual, predicted){
        suppressWarnings(
            caret::confusionMatrix(
                data = predicted %>% as.factor(),
                reference = actual %>% as.factor()
            )
        )
    }

    compose_formula <<- function(role_pk = NULL, role_none = NULL, role_input, role_target){
        X <- role_input %>% setdiff(role_none) %>% setdiff(role_pk) %>% setdiff(role_target)
        y <- role_target

        formula(paste(y, "~", paste(X, collapse = " + ")))
    }

    minmax <<- function(x, lb, ub) {
        stopifnot(lb < ub)
        .minmax <- function(x, lb, ub) min(ub, max(x, lb))
        sapply(x, .minmax, lb = lb, ub = ub)
    }

    return(invisible())
}
