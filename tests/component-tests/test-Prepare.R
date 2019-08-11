context("component test for Prepare abstract class")

test_that("Ingest interface method signatures", {
    expect_class(dp <- Prepare$new(), "Prepare")
    expect_class(dp$.__enclos_env__$private$.ingest, "Ingest")
    expect_an_empty_data.frame(dp$historical_data)
    expect_an_empty_data.frame(dp$new_data)
    expect_an_empty_data.frame(dp$submission_sample)
})

