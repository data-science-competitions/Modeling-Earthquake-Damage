context("unit test utils-modeling")

# Setup -------------------------------------------------------------------
testthat::setup({
    assign("test_env", testthat::test_env(), envir = parent.frame())

    set.seed(921)
    mpg_hat <- mtcars$mpg + rnorm(nrow(mtcars))
    mpg_copy <- mtcars$mpg
    test_env$dataset <- cbind(mpg_hat, mpg_copy, mtcars)
})

# evaluate_model ----------------------------------------------------------
test_that("evaluate_model works", {
    attach(test_env)

    # Some known metrics
    metrics <- c("rmse", "mae", "unknown_metric")
    expect_silent(results <- evaluate_model(data = dataset, truth = "mpg", estimate = "mpg_copy", metrics))
    expect_a_non_empty_data.frame(results)
    expect_table_has_col_names(results, c(".metric", ".estimator", ".estimate"))

    # Only unknown metrics
    metrics <- c("unknown_metric_1", "unknown_metric_2")
    expect_an_empty_data.frame(evaluate_model(data = dataset, truth = "mpg", estimate = "mpg_copy", metrics))
})

test_that("evaluate_model fails because of invalid input arguments", {
    attach(test_env)
    # No metrics
    expect_error(evaluate_model(data = dataset, truth = "mpg", estimate = "mpg_copy"))
})

