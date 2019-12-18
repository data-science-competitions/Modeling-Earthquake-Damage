#' @title Fit One or More Models to a Training Set
#' @param historical_data (`data.frame`) A table where samples are in rows and features are in columns.
#' @param model_formula (`formula`) A symbolic description of the model to be fitted.
#' @note \code{model_fit} has access to variables which were assigned to \code{model_environment}.
#' @return A model object
model_fit <- function(historical_data, model_formula)
{
    ## Split the Data
    set.seed(1558)
    xgb_rsplit <- rsample::group_vfold_cv(historical_data, group = "geo_level_3_id", v = 20)
    xgb_train <- xgb_rsplit %>% get_rsample_training_set(1)
    xgb_test <- xgb_rsplit %>% get_rsample_test_set(1)

    ## Add Weights
    weight_observations <- function(y){
        w <- rep(1, length(y))
        w[y == 1] <- 1
        w[y == 2] <- 1
        w[y == 3] <- 1
        return(w)
    }
    xgb_train_weights <- xgb_train[[role_target]] %>% weight_observations()
    xgb_test_weights <- xgb_test[[role_target]] %>% weight_observations()

    ## Preprocess the Data
    xgb_train <- xgb_train %>% preprocessing_function(weight = xgb_train_weights)
    xgb_test <- xgb_test %>% preprocessing_function(weight = xgb_test_weights)

    ## Fit Model
    set.seed(1451)
    mdl_obj <- xgboost::xgb.train(
        params = params,
        data = xgb_train,
        nrounds = params$nrounds,
        watchlist = list(train = xgb_train, test = xgb_test),
        obj = log_cosh_obj,
        feval = feval_f1,
        maximize = TRUE,
        print_every_n = 10,
        early_stopping_rounds = 50,
        verbose = 2
    )

    return(mdl_obj)
}
