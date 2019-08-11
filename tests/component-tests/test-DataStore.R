context("component test for DataStore object")

test_that("DataStore has access to the project datasets", {
    expect_class(ds <- DataStore$new(), "DataStore")
    expect_class(ds$data_model, "dm")

    expect_class(ds$data_model$historical_data, "data.frame")
    expect_class(ds$data_model$new_data, "data.frame")
    expect_class(ds$data_model$submission_sample, "data.frame")
})

