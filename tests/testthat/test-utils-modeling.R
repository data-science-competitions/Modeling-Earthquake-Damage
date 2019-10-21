context("unit test utils-modeling")

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
    expect_silent(results <- evaluate_model(data = dataset, truth = "mpg", estimate = "mpg_copy"))
    expect_class(results, "data.frame")
    expect_table_has_col_names(results, c(".metric", ".estimator", ".estimate"))
})

