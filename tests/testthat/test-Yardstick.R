context("unit test Yardstick")

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

