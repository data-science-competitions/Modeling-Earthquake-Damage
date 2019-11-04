context("unit test for Yardstick")

# Setup -------------------------------------------------------------------
testthat::setup({
    assign("test_env", testthat::test_env(), envir = parent.frame())
    set.seed(1001)

    mpg_hat <- mtcars$mpg
    test_env$data_reg <- cbind(mpg_hat, mtcars)

    setosa.truth <- as.numeric(iris$Species == "setosa")
    setosa.estimate <- runif(length(setosa.truth))
    test_env$data_cla <- cbind(iris, setosa.truth, setosa.estimate)
})

# constructor -------------------------------------------------------------
test_that("Yardstick constructor works", {
    attach(test_env)
    expect_class(Yardstick$new(data = data_reg, truth = "mpg", estimate = "mpg_hat"), "Yardstick")
})

# known metrics -----------------------------------------------------------
test_that("Yardstick metrics are available", {
    attach(test_env)

    expect_silent(metrics <- Yardstick$new(data = data_reg, truth = "mpg", estimate = "mpg_hat"))
    for(metric in c("rmse", "mae", "rsq", "ccc"))
        expect_a_non_empty_data.frame(metrics[[metric]])
})

# unknown metrics ---------------------------------------------------------
test_that("Yardstick unknown metrics are return NULL", {
    attach(test_env)

    expect_silent(metrics <- Yardstick$new(data = data_reg, truth = "mpg", estimate = "mpg_hat"))
    expect_null(metrics[["unknown_metric"]])
})

# add attributes ----------------------------------------------------------
test_that("Yardstick keys are available", {
    attach(test_env)

    expect_silent(metrics <- Yardstick$new(data = data_reg, truth = "mpg", estimate = "mpg_hat"))
    expect_identical(metrics$keys, c(".metric", ".estimator", ".estimate"))
})

test_that("Yardstick allows to add attributes", {
    attach(test_env)

    expect_silent(metrics <- Yardstick$new(data = data_reg, truth = "mpg", estimate = "mpg_hat"))
    # Add ".set" for 1st time
    expect_silent(metrics$insert_label(key = ".set", value = "train"))
    expect_identical(metrics$keys, c(".set", ".metric", ".estimator", ".estimate"))
    # Add ".set" for 2nd time (shouldn't have duplicates)
    expect_silent(metrics$insert_label(key = ".set", value = "train"))
    expect_identical(metrics$keys, c(".set", ".metric", ".estimator", ".estimate"))
})

test_that("Yardstick adds attributes to results", {
    attach(test_env)

    expect_silent(metrics <- Yardstick$new(data = data_reg, truth = "mpg", estimate = "mpg_hat"))
    expect_silent(metrics$insert_label(key = ".set", value = "train"))
    expect_a_non_empty_data.frame(results <- metrics[["rmse"]])
    expect_table_has_col_names(results, c(".set", ".metric", ".estimator", ".estimate"))
})

# remove attributes -------------------------------------------------------
test_that("Yardstick allows to remove attributes", {
    attach(test_env)

    expect_silent(metrics <- Yardstick$new(data = data_reg, truth = "mpg", estimate = "mpg_hat"))
    expect_silent(metrics$delete_label(key = ".metric"))
    expect_identical(metrics$keys, c(".estimator", ".estimate"))
})

test_that("Yardstick discards attributes from results", {
    attach(test_env)

    expect_silent(metrics <- Yardstick$new(data = data_reg, truth = "mpg", estimate = "mpg_hat"))
    expect_silent(metrics$delete_label(key = ".metric"))
    expect_a_non_empty_data.frame(results <- metrics[["rmse"]])
    expect_identical(colnames(results), c(".estimator", ".estimate"))
})

# set threshold -----------------------------------------------------------
test_that("Yardstick allows to set a threshold value", {
    attach(test_env)

    expect_silent(metrics <- Yardstick$new(data = data_reg, truth = "mpg", estimate = "mpg_hat"))
    expect_silent(metrics$set_threshold(value = 0.5))
})

# set transformation function ---------------------------------------------
test_that("Yardstick allows to set a transformation function", {
    attach(test_env)

    expect_silent(metrics <- Yardstick$new(data = data_reg, truth = "mpg", estimate = "mpg_hat"))
    expect_silent(metrics$set_transformation(fun = function(x) x))
})

# method chaining ---------------------------------------------------------
test_that("Yardstick allows to chain methods", {
    attach(test_env)
    threshold <- 0.5

    expect_silent(
        metrics <- Yardstick$
            new(data = data_reg, truth = "mpg", estimate = "mpg_hat")$
            set_threshold(value = threshold)$
            delete_label(key = ".metric")$
            insert_label(key = ".set", value = "train")
    )

    expect_identical(metrics$keys, c(".set", ".estimator", ".estimate"))
})

# plot lift curve ---------------------------------------------------------
test_that("Yardstick plots lift curve when truth is numeric", {
    attach(test_env)

    expect_silent(metrics <- Yardstick$new(data = data_cla, truth = "setosa.truth", estimate = "setosa.estimate"))
    expect_silent(metrics$set_threshold(value = 0.5))
    expect_class(metrics$plot_lift_curve(), "ggplot")

    data <- data_cla %>% dplyr::rename("truth" = "setosa.truth", "estimate" = "setosa.estimate")
    expect_silent(metrics <- Yardstick$new(data = data, truth = "truth", estimate = "estimate"))
    expect_silent(metrics$set_threshold(value = 0.5))
    expect_class(metrics$plot_lift_curve(), "ggplot")
})

test_that("Yardstick fails to plot lift curve when no threshold is set", {
    attach(test_env)

    expect_silent(metrics <- Yardstick$new(data = data_cla, truth = "setosa.truth", estimate = "setosa.estimate"))
    expect_error(metrics$plot_lift_curve())
})

test_that("Yardstick fails to plot lift curve when truth or estimate are not within the [0,1] boundary", {
    attach(test_env)

    expect_silent(metrics <- Yardstick$new(data = data_cla, truth = "Sepal.Width", estimate = "setosa.estimate"))
    expect_silent(metrics$set_threshold(value = 0.5))
    expect_error(metrics$plot_lift_curve())

    expect_silent(metrics <- Yardstick$new(data = data_cla, truth = "setosa.truth", estimate = "Sepal.Width"))
    expect_silent(metrics$set_threshold(value = 0.5))
    expect_error(metrics$plot_lift_curve())
})

# plot gain curve ---------------------------------------------------------
test_that("Yardstick plots gain curve when truth is numeric", {
    attach(test_env)

    expect_silent(metrics <- Yardstick$new(data = data_cla, truth = "setosa.truth", estimate = "setosa.estimate"))
    expect_silent(metrics$set_threshold(value = 0.5))
    expect_class(metrics$plot_gain_curve(), "ggplot")
})

# Class metrics (hard predictions) ----------------------------------------
test_that("Yardstick calculates accuracy for predicted labels", {
    attach(test_env)

    expect_silent(metrics <- Yardstick$new(data = data_cla, truth = "Species", estimate = "Species"))
    expect_a_non_empty_data.frame(metrics$accuracy)
})

test_that("Yardstick calculates accuracy for predicted probabilities", {
    attach(test_env)
    factor_binary <- function(x) factor(x, levels = 0:1, labels = c("No", "Yes"))
    hard_treshold_function <- function(x) factor_binary(x > 0.5)
    data_cla <- data_cla %>% dplyr::mutate(setosa.truth = factor_binary(setosa.truth))

    expect_silent(metrics <- Yardstick$new(data = data_cla, truth = "setosa.truth", estimate = "setosa.estimate"))
    expect_silent(metrics$set_transformation(fun = hard_treshold_function))
    expect_a_non_empty_data.frame(metrics$accuracy)
})

test_that("Yardstick calculates accuracy for predicted probabilities", {
    attach(test_env)
    factor_binary <- function(x) factor(x, levels = 0:1, labels = c("No", "Yes"))
    hard_treshold_function <- function(x) factor_binary(x > 0.5)

    expect_silent(metrics <- Yardstick$new(data = data_cla, truth = "setosa.truth", estimate = "setosa.estimate"))
    expect_silent(metrics$set_transformation(fun = hard_treshold_function))
    expect_a_non_empty_data.frame(metrics$accuracy)
})

