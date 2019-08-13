context("unit test for PentaModel object")

# Helper Functions --------------------------------------------------------
.create_valid_mock_pentamodel <- function(path){
    writeLines("model_init <- function() NULL", file.path(path, "model_init.R"))
    writeLines("model_fit <- function() NULL", file.path(path, "model_fit.R"))
    writeLines("model_predict <- function() NULL", file.path(path, "model_predict.R"))
    writeLines("model_store <- function() NULL", file.path(path, "model_store.R"))
    writeLines("model_end <- function() NULL", file.path(path, "model_end.R"))
}

.create_invalid_mock_pentamodel <- function(path){
    writeLines("model_init <- function() NULL", file.path(path, "model_init.R"))
    writeLines("model_init <- function() NULL", file.path(path, "model_fit.R"))
    writeLines("model_init <- function() NULL", file.path(path, "model_predict.R"))
    writeLines("model_init <- function() NULL", file.path(path, "model_store.R"))
    writeLines("model_init <- function() NULL", file.path(path, "model_end.R"))
}

# Test Cases -------------------------------------------------------------------
test_that("PentaModel works", {
    model_name <- "mockModel"
    model_path <- file.path(.get_temp_dir(), model_name)
    .delete_and_create_dir(model_path)
    .create_valid_mock_pentamodel(model_path)

    expect_class(mdl <- PentaModel$new(path = model_path), "PentaModel")
    expect_identical(mdl$model_name, model_name)
    expect_identical(mdl$model_path, model_path)

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
