context("unit test for utils-R6")

# Setup -------------------------------------------------------------------
testthat::setup({
    assign("test_env", testthat::test_env(), envir = parent.frame())
    test_env$shared_env <- new.env()
    test_env$key <- "fruit"
    test_env$value <- "Apple"
    test_env$new_value <- "Papaya"
})

# Shared Environment CRUD API ---------------------------------------------
test_that("CRUD API simple case", {
    attach(test_env)

    expect_null(.create_shared_env(key, value))
    expect_identical(.read_shared_env(key), value)
    expect_null(.update_shared_env(key, new_value))
})

# reading non existing object
