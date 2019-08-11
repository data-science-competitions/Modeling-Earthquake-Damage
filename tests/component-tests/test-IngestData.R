context("component test for Ingest concrete class (IngestData)")

test_that("IngestData implements its interface method signatures", {
    expect_class(db <- IngestData$new(), "Ingest")
    expect_a_non_empty_data.frame(db$historical_data)
    expect_a_non_empty_data.frame(db$new_data)
    expect_a_non_empty_data.frame(db$submission_format)
})
