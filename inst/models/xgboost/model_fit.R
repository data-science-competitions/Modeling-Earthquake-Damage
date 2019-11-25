#' @title Fit One or More Models to a Training Set
#' @param historical_data (`data.frame`) A table where samples are in rows and features are in columns.
#' @param model_formula (`formula`) A symbolic description of the model to be fitted.
#' @note \code{model_fit} has access to variables which were assigned to \code{model_environment}.
#' @return A model object
model_fit <- function(historical_data, model_formula)
{
    set.seed(1558)
    xgb_rsplit <- rsample::initial_split(historical_data, 0.95)
    xgb_train <- xgb_rsplit %>% get_rsample_training_set(1) %>% preprocessing_function()
    xgb_test <- xgb_rsplit %>% get_rsample_test_set(1) %>% preprocessing_function()

    set.seed(1451)
    mdl_obj <- xgboost::xgb.train(
        params = params,
        data = xgb_train,
        nrounds = params$nrounds,
        watchlist = list(train = xgb_train, test = xgb_test),
        obj = log_cosh_obj,
        feval = feval_f1,
        print_every_n = 10,
        early_stopping_rounds = 50,
        maximize = TRUE,
        verbose = 1#getOption("verbose")
    )

    return(mdl_obj)
}
