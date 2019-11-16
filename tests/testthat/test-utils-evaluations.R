context("unit test for utils-evaluations")

# Setup -------------------------------------------------------------------
testthat::setup({
    assign("test_env", testthat::test_env(), envir = parent.frame())
    set.seed(1001)

    test_env$data <- mtcars
    test_env$model <- lm(mpg ~ ., test_env$data)
    test_env$explainer_obj <-
        DALEX::explain(model = test_env$model,
                       data = test_env$data %>% dplyr::select(-mpg),
                       y = test_env$data %>% .$mpg,
                       verbose = FALSE)
})

# ExplainerYardstick ------------------------------------------------------
test_that("ExplainerYardstick instantiate Yardstick", {
    attach(test_env)
    expect_class(ExplainerYardstick(explainer_obj), "Yardstick")
})

test_that("ExplainerYardstick returns certain information", {
    attach(test_env)
    expect_silent(Yardstick_obj <- ExplainerYardstick(explainer_obj))
    expect_table_has_col_names(
        Yardstick_obj$all_numeric_metrics,
        c(".n", ".label", ".model")
    )
})


