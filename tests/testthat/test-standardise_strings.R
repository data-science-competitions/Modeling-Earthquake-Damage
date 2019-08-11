context("unit tests for standardise_strings")

# standardise_strings ----------------------------------------------------------
test_that("standardise_strings fails when given invalid input arguments", {
    # No input arguments
    expect_error(standardise_strings())
    # No allowed_letters
    expect_error(standardise_strings("x", allowd_special_characters = "dots"))
    # No allowd_special_characters
    expect_error(standardise_strings("x", allowed_letters = "anycase"))
    # allowed_letters doesn't match any option
    expect_error(standardise_strings("x", "nocase", "dots"))
    # allowd_special_characters doesn't match any option
    expect_error(standardise_strings("x", "anycase", "everything"))
})

test_that("standardise_strings detects sentinel values and prompts an error", {
    expect_error(standardise_strings(1:5, "uppercase", "underscores"))
    expect_error(standardise_strings(data.frame(X = letters),"uppercase", "underscores"))

    expect_error(standardise_strings(" ", "uppercase", "underscores"))
    expect_error(standardise_strings("_", "uppercase", "underscores"))
    expect_error(standardise_strings("__", "uppercase", "underscores"))
    expect_error(standardise_strings("_ _", "uppercase", "underscores"))
    expect_error(standardise_strings("-", "uppercase", "hyphens"))
    expect_error(standardise_strings("_-", "uppercase", "hyphens"))
    expect_error(standardise_strings("_-", "uppercase", "hyphens"))
})

test_that("standardise_strings works when given valid input arguments", {
    expect_equal(standardise_strings("Year", "uppercase", "underscores"), "YEAR")
    expect_equal(standardise_strings("FinancialYear", "uppercase", "underscores"), "FINANCIAL_YEAR")
    expect_equal(standardise_strings("LongE-N", "uppercase", "underscores"), "LONG_E_N")
    expect_equal(standardise_strings("Avg_RWP_rut/yr", "uppercase", "underscores"), "AVG_RWP_RUT_YR")
    expect_equal(standardise_strings("Reference #", "uppercase", "underscores"), "REFERENCE")

    expect_equal(standardise_strings("Year", "uppercase", "hyphens"), "YEAR")
    expect_equal(standardise_strings("FinancialYear", "uppercase", "hyphens"), "FINANCIAL-YEAR")
    expect_equal(standardise_strings("LongE-N", "uppercase", "hyphens"), "LONG-E-N")
    expect_equal(standardise_strings("Avg_RWP_rut/yr", "uppercase", "hyphens"), "AVG-RWP-RUT-YR")
    expect_equal(standardise_strings("Reference #", "uppercase", "hyphens"), "REFERENCE")

    expect_equal(standardise_strings("Year", "uppercase", "dots"), "YEAR")
    expect_equal(standardise_strings("FinancialYear", "uppercase", "dots"), "FINANCIAL.YEAR")
    expect_equal(standardise_strings("LongE-N", "uppercase", "dots"), "LONG.E.N")
    expect_equal(standardise_strings("Avg_RWP_rut/yr", "uppercase", "dots"), "AVG.RWP.RUT.YR")
    expect_equal(standardise_strings("Reference #", "uppercase", "dots"), "REFERENCE")
})

test_that("standardise_strings works when given letters and numbers", {
    expect_equal(standardise_strings("Run4YourLife", "anycase", "underscores"), "Run_4_Your_Life")
    expect_equal(standardise_strings("LwpIRI_Run4", "anycase", "underscores"), "Lwp_IRI_Run_4")
    expect_equal(standardise_strings("Call100Today", "anycase", "underscores"), "Call_100_Today")
})

test_that("standardise_strings works when given formulas",{
    ### Plus sign stays the same
    expect_equal(standardise_strings(
        "TotalPave+SurfThick", "uppercase", "underscores"),
        "TOTAL_PAVE+SURF_THICK")
    ### Tilde sign stays the same
    expect_equal(standardise_strings(
        "SHEAR_FAILURE~TMS_AADT_2011", "uppercase", "underscores"),
        "SHEAR_FAILURE~TMS_AADT_2011")
})

test_that("standardise_strings works with vectors", {
    expect_equal(standardise_strings(letters, "uppercase", "underscores"), LETTERS)
    expect_error(standardise_strings(c(" ", letters), "uppercase", "underscores"))
})
