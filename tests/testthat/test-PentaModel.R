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

    # expect_null(mdl$model_predict())

    expect_null(mdl$model_store())
    expect_null(mdl$model_end())
})

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

test_that("Setters fail due to missing input arguments", {
    model_name <- "mockModel"
    model_path <- file.path(.get_temp_dir(), model_name)
    .delete_and_create_dir(model_path)
    .create_valid_mock_pentamodel(model_path)

    expect_class(mdl <- PentaModel$new(path = model_path), "PentaModel")
    expect_error(mdl$set_historical_data())
    expect_error(mdl$set_new_data())
})
