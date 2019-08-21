context("unit test for PentaModel object")

test_that("PentaModel loads model component to an isolated environment", {
    model_name <- "mockModel"
    model_path <- file.path(.get_temp_dir(), model_name)
    .delete_and_create_dir(model_path)
    .create_valid_mock_pentamodel(model_path)

    expect_class(mdl <- PentaModel$new(path = model_path), "PentaModel")
    expect_identical(mdl$model_name, model_name)
    expect_identical(mdl$model_path, model_path)
    expect_false(exists("model_init"))
    expect_false(exists("model_fit"))
    expect_false(exists("model_predict"))
    expect_false(exists("model_store"))
    expect_false(exists("model_end"))
})

test_that("PentaModel workflow is flawless", {
    model_name <- "mockModel"
    model_path <- file.path(.get_temp_dir(), model_name)
    .delete_and_create_dir(model_path)
    .create_valid_mock_pentamodel(model_path)
    expect_silent(mdl <- PentaModel$new(path = model_path))

    historical_data <- mtcars[1:22,]
    new_data <- mtcars[23:32,]
    mdl_object <- lm(mpg ~ ., historical_data)
    y_hat <- predict(mdl_object, new_data)

    expect_null(mdl$set_historical_data(historical_data))
    expect_null(mdl$set_new_data(new_data))

    expect_null(mdl$model_init())

    expect_null(mdl$model_fit())
    expect_equal(mdl$model_object, mdl_object)

    expect_null(mdl$model_predict())
    expect_equal(mdl$response, y_hat)

    expect_null(mdl$model_store())
    expect_null(mdl$model_end())
})

test_that("PentaModel can be preset with a model object", {
    model_name <- "mockModel"
    model_path <- file.path(.get_temp_dir(), model_name)
    .delete_and_create_dir(model_path)
    .create_valid_mock_pentamodel(model_path)
    expect_silent(mdl <- PentaModel$new(path = model_path))

    historical_data <- mtcars[1:22,]
    new_data <- mtcars[23:32,]
    mdl_object <- lm(mpg ~ ., historical_data)
    y_hat <- predict(mdl_object, new_data)

    expect_null(mdl$set_model(mdl_object))
    expect_identical(mdl$model_object, mdl_object)

    expect_null(mdl$set_new_data(new_data))
    expect_null(mdl$model_predict())
    expect_equal(mdl$response, y_hat)
})

test_that("PentaModel formula components work", {
    model_name <- "mockModel"
    model_path <- file.path(.get_temp_dir(), model_name)
    .delete_and_create_dir(model_path)
    .create_valid_mock_pentamodel(model_path)
    expect_silent(mdl <- PentaModel$new(path = model_path))
    mtcars <- mtcars %>% tibble::rownames_to_column("UID")

    expect_null(mdl$set_role_pk("UID"))
    expect_null(mdl$set_role_none("wt"))
    expect_null(mdl$set_role_input("cyl"))
    expect_null(mdl$set_role_target("mpg"))
    expect_equal(mdl$model_formula, formula(mpg ~ cyl))
})

test_that("PentaModel workflow given var roles", {
    model_name <- "mockModel"
    model_path <- file.path(.get_temp_dir(), model_name)
    .delete_and_create_dir(model_path)
    .create_valid_mock_pentamodel(model_path)
    expect_silent(mdl <- PentaModel$new(path = model_path))

    historical_data <- mtcars[1:22,]
    new_data <- mtcars[23:32,]
    mdl_object <- lm(mpg ~ cyl, historical_data)

    expect_null(mdl$set_historical_data(historical_data))
    expect_null(mdl$set_new_data(new_data))

    expect_null(mdl$set_role_input("cyl"))
    expect_null(mdl$set_role_target("mpg"))
    expect_null(mdl$model_init())
    expect_null(mdl$model_fit())
    # expect_identical(mdl$model_object, mdl_object)

    # expect_null(mdl$model_predict())
    # expect_null(mdl$model_store())
    # expect_null(mdl$model_end())
})

# Produce Errors ---------------------------------------------------------------
test_that("PentaModel fails due to missing input arguments / files", {
    model_name <- "mockModel"
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

test_that("PentaModel prompt an error when model_fit has no historical_data", {
    model_name <- "mockModel"
    model_path <- file.path(.get_temp_dir(), model_name)
    .delete_and_create_dir(model_path)
    .create_valid_mock_pentamodel(model_path)
    expect_silent(mdl <- PentaModel$new(path = model_path))

    historical_data <- mtcars[1:22,]
    new_data <- mtcars[23:32,]
    mdl_object <- lm(mpg ~ ., historical_data)
    y_hat <- predict(mdl_object, new_data)

    expect_error(mdl$model_fit())
})

test_that("PentaModel promts an error when model_predict has no model/new_data", {
    model_name <- "mockModel"
    model_path <- file.path(.get_temp_dir(), model_name)
    .delete_and_create_dir(model_path)
    .create_valid_mock_pentamodel(model_path)
    expect_silent(mdl <- PentaModel$new(path = model_path))

    historical_data <- mtcars[1:22,]
    new_data <- mtcars[23:32,]
    mdl_object <- lm(mpg ~ ., historical_data)

    expect_error(mdl$model_predict())

    expect_null(mdl$set_new_data(new_data))
    expect_error(mdl$model_predict())
})

test_that("PentaModel model_predict outputs fewer predictions than there are in the new_data", {
    na.action <- getOption("na.action")
    on.exit(options(na.action = na.action))
    model_name <- "mockModel"
    model_path <- file.path(.get_temp_dir(), model_name)
    .delete_and_create_dir(model_path)
    .create_valid_mock_pentamodel(model_path)
    expect_silent(mdl <- PentaModel$new(path = model_path))

    historical_data <- mtcars[1:22,]
    new_data <- mtcars[23:32,]
    new_data[1, 2] <- NA
    mdl_object <- lm(mpg ~ ., historical_data)

    expect_null(mdl$set_new_data(new_data))
    expect_null(mdl$set_model(mdl_object))

    options(na.action = "na.pass")
    expect_error(mdl$model_predict())

    options(na.action = "na.exclude")
    expect_error(mdl$model_predict())
})

