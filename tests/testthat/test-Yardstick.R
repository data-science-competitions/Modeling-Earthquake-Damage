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


