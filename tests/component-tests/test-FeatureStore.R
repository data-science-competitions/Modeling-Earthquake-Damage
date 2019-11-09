context("component test for FeatureStore")

testthat::setup({
    assign("test_env", testthat::test_env(), envir = parent.frame())
})

test_that("FeatureStore can be instantiated", {
    attach(test_env)
    expect_class(test_env$obj <- FeatureStore$new(), "FeatureStore")
})

test_that("FeatureStore has access to the project datasets", {
    attach(test_env)
    expect_class(obj$tidy_data, "data.frame")
    expect_table_has_col_names(obj$tidy_data, c("source", "building_id"))
})

test_that("FeatureStore has access to the new feature sets", {
    attach(test_env)
    expect_class(obj$geo_features, "data.frame")
    expect_table_has_col_names(obj$geo_features, c("building_id"))
})

testthat::teardown(test_env <- NULL)

