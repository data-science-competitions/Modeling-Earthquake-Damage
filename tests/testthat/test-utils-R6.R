context("unit test for utils-R6")

# Setup -------------------------------------------------------------------
testthat::setup({
    assign("test_env", testthat::test_env(), envir = parent.frame())
    test_env$shared_env <- new.env()
    test_env$key_without_value <- "dogs"
    test_env$key <- "cats"
    test_env$value <- "Persian"
    test_env$new_value <- "Sphynx"
})

# Shared Environment CRUD API ---------------------------------------------
test_that("CRUD creates a new object", {
    attach(test_env)

    expect_null(.create_shared_env(key, value)) # object doesn't exist
    expect_null(.create_shared_env(key, value)) # object exists
})

test_that("CRUD reads an object", {
    attach(test_env)

    expect_null(.create_shared_env(key, value))
    expect_identical(.read_shared_env(key), value)
    expect_identical(.read_shared_env(key_without_value), NULL)
})

test_that("CRUD updates an object", {
    attach(test_env)

    expect_null(.update_shared_env(key, value)) # object doesn't exist
    expect_null(.update_shared_env(key, new_value)) # object exists
    expect_identical(.read_shared_env(key), new_value)
})

test_that("CRUD deletes an object", {
    attach(test_env)

    expect_null(.create_shared_env(key, value))
    expect_null(.delete_shared_env(key))
    expect_identical(.read_shared_env(key), NULL)

    expect_null(.delete_shared_env(key_without_value))
    expect_silent(.delete_shared_env(key_without_value))
})

testthat::teardown(test_env <- NULL)
