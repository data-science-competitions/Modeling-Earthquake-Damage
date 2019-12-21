#' @title Fit One or More Models to a Training Set
#' @param historical_data (`data.frame`) A table where samples are in rows and features are in columns.
#' @param model_formula (`formula`) A symbolic description of the model to be fitted.
#' @note \code{model_fit} has access to variables which were assigned to \code{model_environment}.
#' @return A model object
model_fit <- function(historical_data, model_formula)
{
    ## Split the Data
    set.seed(1558)
    n <- nrow(historical_data)
    train_index <- sample(n, n - 1e3)
    xgb_train <- historical_data[+train_index, ]
    xgb_test <- historical_data[-train_index, ]

    ## Add Weights
    weight_observations <- function(.data){
        w <- rep(1, nrow(.data))
        # w <- ifelse(.data$geo_level_3_id_in_test, 1, 0.707)
        return(w)
    }
    xgb_train_weights <- xgb_train %>% weight_observations()
    xgb_test_weights <- xgb_test %>% weight_observations()

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
