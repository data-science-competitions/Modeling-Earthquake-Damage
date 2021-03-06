context("component test for PrepareData")

test_that("PrepareDAO implements its interface method signatures", {
    expect_class(dp <- PrepareData$new(), "Prepare")
    expect_a_non_empty_data.frame(dp$historical_data)
    expect_a_non_empty_data.frame(dp$new_data)
    expect_a_non_empty_data.frame(dp$submission_format)
})
