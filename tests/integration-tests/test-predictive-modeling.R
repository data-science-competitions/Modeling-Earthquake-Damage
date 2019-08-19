context("integration tests for predictive modeling")

test_that("Data Pipeline and PentaModel are working together", {
    model_name <- "mockModel"
    model_path <- file.path(.get_temp_dir(), model_name)
    .delete_and_create_dir(model_path)
    .create_valid_mock_pentamodel(model_path)

    expect_class(ds <- DataStore$new(), "DataStore")
    expect_class(pm <- PentaModel$new(model_path), "PentaModel")

    expect_null(pm$set_historical_data(ds$data_model$historical_data))
    expect_null(pm$set_new_data(ds$data_model$new_data))

    expect_null(pm$model_init())
    expect_null(pm$model_fit())

    expect_class(pm$model_object, "lm")
})
