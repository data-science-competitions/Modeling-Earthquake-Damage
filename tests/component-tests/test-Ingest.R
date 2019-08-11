context("component test for Ingest abstract class")

test_that("Ingest interface method signatures", {
    expect_class(db <- Ingest$new(), "Ingest")
    expect_class(db <- Ingest$new(path = tempdir()), "Ingest")
    expect_an_empty_data.frame(db$historical_data)
    expect_an_empty_data.frame(db$new_data)
    expect_an_empty_data.frame(db$submission_sample)
})

