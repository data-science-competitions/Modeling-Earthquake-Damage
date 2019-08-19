context("unit test for PentaModel object")

test_that("PentaModel works", {
    model_name <- "mockModel"
    model_path <- file.path(.get_temp_dir(), model_name)
    .delete_and_create_dir(model_path)
    .create_valid_mock_pentamodel(model_path)

    expect_class(mdl <- PentaModel$new(path = model_path), "PentaModel")
    expect_identical(mdl$model_name, model_name)
    expect_identical(mdl$model_path, model_path)

    expect_null(mdl$set_historical_data(mtcars[1:22,]))
    expect_null(mdl$set_new_data(mtcars[23:32,]))

    expect_null(mdl$model_init())
    expect_null(mdl$model_fit())
    expect_null(mdl$model_predict())
    expect_null(mdl$model_store())
    expect_null(mdl$model_end())

    expect_false(exists("model_init"))
    expect_false(exists("model_fit"))
    expect_false(exists("model_predict"))
    expect_false(exists("model_store"))
    expect_false(exists("model_end"))
})

test_that("PentaModel public methods work", {
    model_name <- "mockModel"
    model_path <- file.path(.get_temp_dir(), model_name)
    .delete_and_create_dir(model_path)
    .create_valid_mock_pentamodel(model_path)

    historical_data <- mtcars[1:22,]
    new_data <- mtcars[23:32,]
    model_obj <- lm(mpg ~ ., historical_data)

    expect_class(mdl <- PentaModel$new(path = model_path), "PentaModel")
    expect_null(mdl$set_model(model_obj))
    expect_null(mdl$set_historical_data(historical_data))
    expect_null(mdl$set_new_data(new_data))
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
