context("component test for DataStore")

testthat::setup({
    assign("test_env", testthat::test_env(), envir = parent.frame())
})

test_that("DataStore implements its interface method signatures", {
    attach(test_env)
    expect_class(test_env$obj <- DataStore$new(), "DataStore")
})

test_that("DataStore has access to the project datasets", {
    attach(test_env)
    expect_class(obj$data_model, "dm")

    expect_class(obj$data_model$historical_data, "data.frame")
    expect_class(obj$data_model$new_data, "data.frame")
    expect_class(obj$data_model$submission_format, "data.frame")
})

testthat::teardown(test_env <- NULL)

