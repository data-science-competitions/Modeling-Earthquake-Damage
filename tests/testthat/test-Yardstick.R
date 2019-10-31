context("unit test for Yardstick")

# Setup -------------------------------------------------------------------
testthat::setup({
    assign("test_env", testthat::test_env(), envir = parent.frame())
    mpg_hat <- mtcars$mpg
    test_env$data <- cbind(mpg_hat, mtcars)
})

# constructor -------------------------------------------------------------
test_that("Yardstick constructor works", {
    attach(test_env)
    expect_class(Yardstick$new(data = data, truth = "mpg", estimate = "mpg_hat"), "Yardstick")
})

# known metrics -----------------------------------------------------------
test_that("Yardstick metrics are available", {
    attach(test_env)

    expect_silent(metrics <- Yardstick$new(data = data, truth = "mpg", estimate = "mpg_hat"))
    for(metric in c("rmse", "mae", "rsq", "ccc"))
        expect_a_non_empty_data.frame(metrics[[metric]])
})

# unknown metrics ---------------------------------------------------------
test_that("Yardstick unknown metrics are return NULL", {
    attach(test_env)

    expect_silent(metrics <- Yardstick$new(data = data, truth = "mpg", estimate = "mpg_hat"))
    expect_null(metrics[["unknown_metric"]])
})

# add attributes ----------------------------------------------------------
test_that("Yardstick keys are available", {
    attach(test_env)

    expect_silent(metrics <- Yardstick$new(data = data, truth = "mpg", estimate = "mpg_hat"))
    expect_identical(metrics$keys, c(".metric", ".estimator", ".estimate"))
})

test_that("Yardstick allows to add attributes", {
    attach(test_env)

    expect_silent(metrics <- Yardstick$new(data = data, truth = "mpg", estimate = "mpg_hat"))
    # Add ".set" for 1st time
    expect_silent(metrics$insert_label(key = ".set", value = "train"))
    expect_identical(metrics$keys, c(".set", ".metric", ".estimator", ".estimate"))
    # Add ".set" for 2nd time (shouldn't have duplicates)
    expect_silent(metrics$insert_label(key = ".set", value = "train"))
    expect_identical(metrics$keys, c(".set", ".metric", ".estimator", ".estimate"))
})

test_that("Yardstick adds attributes to results", {
    attach(test_env)

    expect_silent(metrics <- Yardstick$new(data = data, truth = "mpg", estimate = "mpg_hat"))
    expect_silent(metrics$insert_label(key = ".set", value = "train"))
    expect_a_non_empty_data.frame(results <- metrics[["rmse"]])
    # expect_table_has_col_names(results, c(".set", ".metric", ".estimator", ".estimate"))
})

