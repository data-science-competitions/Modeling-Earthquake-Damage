context("unit test for foo")

test_that("foo fails when given invalid input arguments", {
    # No input arguments
    expect_error(foo())
    # Multiple names
    expect_error(foo(name = LETTERS, age = 26))
    # Negative age
    expect_error(foo(name = "Bilbo Baggins", age = -51))
})

test_that("foo works when given valid input arguments", {
    ## Avoid scientific notation
    expected_msg <- "Earth is 4543000000 years old"
    expect_identical(foo(name = "Earth", age = 4.543e9), expected_msg)
})

test_that("foo is a dummy function", {
    testthat::skip("Delete 'R/foo.R' and 'tests/testthat/test-foo.R'")
})
