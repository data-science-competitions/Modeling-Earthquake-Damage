context("unit test for package hooks")

test_that("DataStore fulfils its datasets assumptions", {
    expect_false(is.null(getOption("path_project")))
})
