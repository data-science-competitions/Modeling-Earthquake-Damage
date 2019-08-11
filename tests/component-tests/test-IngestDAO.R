context("component test for Ingest concrete class (DAO)")

test_that("IngestData implements its interface method signatures", {
    expect_class(db <- IngestDAO$new(path = tempdir()), "Ingest")
    expect_a_non_empty_data.frame(db$historical_data)
    expect_a_non_empty_data.frame(db$new_data)
    expect_a_non_empty_data.frame(db$submission_sample)
})
