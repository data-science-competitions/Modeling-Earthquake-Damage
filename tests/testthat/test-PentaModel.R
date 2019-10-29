context("unit test for PentaModel")

# Setup -------------------------------------------------------------------
testthat::setup({
    assign("test_env", testthat::test_env(), envir = parent.frame())

    model_name <- "validMockModel"
    model_path <- file.path(.get_temp_dir(), model_name)

    .delete_and_create_dir(model_path)
    .create_valid_mock_pentamodel(model_path)

    get_fresh_model <- function(){
        suppressWarnings(rm(list = model_name))
        expect_silent({
            valid_mdl <- PentaModel$new(path = model_path)
            valid_mdl$set_historical_data(mtcars[1:22,])
            valid_mdl$set_new_data(mtcars[23:32,])
            valid_mdl$set_role_none("wt")
            valid_mdl$set_role_input("cyl")
            valid_mdl$set_role_target("mpg")
        })
        return(valid_mdl)
    }

    test_env$model_name <- model_name
    test_env$model_path <- model_path
    test_env$get_fresh_model <- get_fresh_model
})

# Set/Get Shared Objects --------------------------------------------------
test_that("PentaModel allows to store/retrieve link_function", {
    attach(test_env)
    mdl <- get_fresh_model()

    # Query the Default link function
    Identity <- function(x) x
    expect_equal(mdl$link_function, Identity)

    # Change link function
    ReLU  <- function(x) max(0,x)
    expect_null(mdl$set_link_function(ReLU))

    # Query updated link function
    expect_identical(mdl$link_function, ReLU)
})

test_that("PentaModel allows to store/retrieve predict_function", {
    attach(test_env)
    mdl <- get_fresh_model()

    # Query the Default predict function
    generic_predict <- function(model, new_data) predict(model, new_data)
    expect_equal(mdl$predict_function, generic_predict)

    # Change predict function
    predict_CI  <- function(model, new_data) predict(model, new_data, interval = "prediction")
    expect_null(mdl$set_predict_function(predict_CI))

    # Query updated predict function
    expect_identical(mdl$predict_function, predict_CI)
})

# Successful Modeling Process ---------------------------------------------
test_that("PentaModel loads model component to an isolated environment", {
    model_name <- "validMockModel"
    model_path <- file.path(.get_temp_dir(), model_name)

    expect_class(mdl <- PentaModel$new(path = model_path), "PentaModel")
    expect_identical(mdl$model_name, model_name)
    expect_identical(mdl$model_path, model_path)
    expect_false(exists("model_init"))
    expect_false(exists("model_fit"))
    expect_false(exists("model_predict"))
    expect_false(exists("model_store"))
    expect_false(exists("model_end"))
})

test_that("PentaModel composes variable roles into formula", {
    attach(test_env)
    mdl <- get_fresh_model()

    expect_null(mdl$set_role_pk(NULL))
    expect_null(mdl$set_role_none("wt"))
    expect_null(mdl$set_role_input("cyl"))
    expect_null(mdl$set_role_target("mpg"))
    expect_equal(mdl$model_formula, formula(mpg ~ cyl))
})

# model_init --------------------------------------------------------------
test_that("PentaModel model_init appends its environment to the object environment", {
    attach(test_env)
    mdl <- get_fresh_model()
    expect_null(mdl$model_init())
    expect_subset("params", ls(mdl$model_environment, all.names=TRUE))
})

# model_fit ---------------------------------------------------------------

# model_predict -----------------------------------------------------------
test_that("PentaModel uses a given primary key", {
    attach(test_env)
    mdl <- get_fresh_model()

    historical_data <- mtcars[1:22,] %>% tibble::rownames_to_column("uid")
    new_data <- mtcars[23:32,] %>% tibble::rownames_to_column("uid")

    expect_null({
        mdl$set_role_pk("uid")
        mdl$set_historical_data(historical_data)
        mdl$set_new_data(new_data)
        mdl$model_fit()
        mdl$model_predict()
    })

    expect_table_has_col_names(mdl$response, "uid")
})

test_that("PentaModel can be preset with a model object", {
    attach(test_env)
    mdl <- get_fresh_model()

    historical_data <- mtcars[1:22,]
    new_data <- mtcars[23:32,]
    mdl_object <- lm(mpg ~ ., historical_data)
    y_hat <- predict(mdl_object, new_data)

    expect_null(mdl$set_model(mdl_object))
    expect_identical(mdl$model_object, mdl_object)

    expect_null(mdl$set_new_data(new_data))
    expect_null(mdl$model_predict())
    expect_equal(mdl$response[["response"]], y_hat %>% unname())
})

test_that("PentaModel can be handle multiple column response", {
    attach(test_env)
    mdl <- get_fresh_model()
    historical_data <- mtcars[1:22,]
    new_data <- mtcars[23:32,]

    expect_silent({
        prediction_intervals <- function(mdl_object, new_data) predict(mdl_object, new_data,  interval = "prediction")
        mdl$set_predict_function(prediction_intervals)
        mdl$set_new_data(new_data)
        mdl$model_fit()
    })

    expect_null(mdl$model_predict())

    response <- prediction_intervals(mdl$model_object, new_data)
    expect_equivalent(
        mdl$response %>% dplyr::select(-rowid),
        response %>% as.data.frame(row.names = FALSE)
    )
})

test_that("PentaModel composes row ids in the absence of role_pk", {
    attach(test_env)
    mdl <- get_fresh_model()

    expect_null(mdl$set_role_pk(NULL))
    expect_null(mdl$model_init())
    expect_null(mdl$model_fit())
    expect_null(mdl$model_predict())
    expect_a_non_empty_data.frame(mdl$response)
    expect_true(colnames(mdl$response)[1] == "rowid")
    expect_true(colnames(mdl$response)[2] == "response")
    expect_class(mdl$response$rowid, "character")
})

# model_store -------------------------------------------------------------
test_that("PentaModel fetches model_store with access to the model environment", {
    attach(test_env)
    mdl <- get_fresh_model()
    expect_null(mdl$model_init())
    expect_null(mdl$model_fit())

    expect_null(mdl$model_store())
    expect_identical(mdl$model_environment$artifacts, letters)
})

# model_end ---------------------------------------------------------------

# Unsuccessful model_fit --------------------------------------------------
test_that("PentaModel prompts an error when model_fit has no role_input", {
    attach(test_env)
    mdl <- get_fresh_model()
    mdl$set_role_input(NULL)
    expect_error(mdl$model_fit())
})

test_that("PentaModel prompts an error when model_fit has no role_target", {
    attach(test_env)
    mdl <- get_fresh_model()
    mdl$set_role_target(NULL)
    expect_error(mdl$model_fit())
})

test_that("PentaModel prompts an error when model_fit has many role_target", {
    attach(test_env)
    mdl <- get_fresh_model()

    mdl$set_role_target(letters)
    expect_error(mdl$model_fit())
})

test_that("PentaModel prompts an error when model_fit has no historical_data", {
    attach(test_env)
    mdl <- get_fresh_model()

    mdl$set_historical_data(NULL)
    expect_error(mdl$model_fit())
})

test_that("PentaModel prompts an error when model_fit has role_input which are not in historical_data", {
    attach(test_env)
    mdl <- get_fresh_model()

    mdl$set_role_input(c("hp", "dummy"))
    expect_error(mdl$model_fit())
})

# Unsuccessful model_predict ----------------------------------------------
test_that("PentaModel promts an error when model_predict has no model", {
    attach(test_env)
    mdl <- get_fresh_model()

    mdl$set_model(NULL)
    expect_error(mdl$model_predict())
})

test_that("PentaModel promts an error when model_predict has no new_data", {
    attach(test_env)
    mdl <- get_fresh_model()

    mdl$model_fit()
    mdl$set_new_data(NULL)
    expect_error(mdl$model_predict())
})

test_that("PentaModel prompts an error when model_predict has role_input which are not in new_data", {
    attach(test_env)
    mdl <- get_fresh_model()

    mdl$model_fit()
    mdl$set_role_input(c("hp", "dummy"))
    expect_error(mdl$model_predict())
})

test_that("PentaModel model_predict outputs fewer predictions than there are in the new_data", {
    attach(test_env)
    mdl <- get_fresh_model()

    na.action <- getOption("na.action")
    on.exit(options(na.action = na.action))
    predict_function <- function(model_object, new_data) predict(model_object, new_data, na.action = .Options$na.action)

    new_data <- mtcars[23:32,]
    new_data[1, 2] <- NA
    mdl$set_new_data(new_data)
    expect_null(mdl$model_fit())
    expect_null(mdl$set_predict_function(predict_function))

    options(na.action = "na.pass")
    expect_error(mdl$model_predict())
    error_na.pass <- capture_error(mdl$model_predict())

    options(na.action = "na.exclude")
    expect_error(mdl$model_predict())
    error_na.exclude <- capture_error(mdl$model_predict())

    expect_not_identical(error_na.pass, error_na.exclude)
})

# Unsuccessful Modeling Process -------------------------------------------
test_that("PentaModel prompts an error when roles overlap", {
    attach(test_env)
    mdl <- get_fresh_model()

    mdl$set_role_none("wt")
    mdl$set_role_target("mpg")
    expect_error(mdl$set_role_input("wt"))
    expect_error(mdl$set_role_input(c("wt", "mpg")))
})

test_that("PentaModel fails due to missing input arguments / files", {
    model_name <- "invalidMockModel"
    model_path <- file.path(.get_temp_dir(), model_name)
    .delete_and_create_dir(model_path)

    ## No model path
    expect_error(PentaModel$new())
    ## No model components in model path
    expect_error(PentaModel$new(path = model_path))
    ## Invalid component
    .create_invalid_mock_pentamodel(model_path)
    expect_error(PentaModel$new(path = model_path))
})

testthat::teardown(test_env <- NULL)
