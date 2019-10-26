context("unit test for utils-R6")

# Setup -------------------------------------------------------------------
testthat::setup({
    assign("test_env", testthat::test_env(), envir = parent.frame())

    set.seed(921)
    mpg_hat <- mtcars$mpg + rnorm(nrow(mtcars))
    mpg_copy <- mtcars$mpg
    test_env$dataset <- cbind(mpg_hat, mpg_copy, mtcars)
})

# Shared Environment CRUD API ---------------------------------------------
test_that("CRUD API: .create_shared_env works", {
    attach(test_env)

})


