context("integration tests for dummy")
#
# Given an empty integration-tests folder,
# When testthat.R is called,
# Then the execution fails.
#
# test-dummy.R exists to circumvent this restriction. Otherwise this file should
# be deleted.
#
test_that("test-dummy.R exists", {
    testthat::skip("Please read test-dummy.R notes")
})
