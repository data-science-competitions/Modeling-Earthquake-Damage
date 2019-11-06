context("unit test for Yardstick")

# Setup -------------------------------------------------------------------
testthat::setup({
    assign("test_env", testthat::test_env(), envir = parent.frame())
    set.seed(1001)

    test_env$class_metrics <- c("accuracy")
    test_env$probability_metrics <- c()
    test_env$numeric_metrics <- c("rmse", "mae", "rsq", "ccc")

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
test_that("Yardstick known class metrics work", {
    attach(test_env)

    expect_silent(metrics <- Yardstick$new(data = data_cla, truth = "Species", estimate = "Species"))
    for(metric in class_metrics)
        expect_a_non_empty_data.frame(metrics[[metric]])
})

test_that("Yardstick known numeric metrics work", {
    attach(test_env)

    expect_silent(metrics <- Yardstick$new(data = data_reg, truth = "mpg", estimate = "mpg_hat"))
    for(metric in numeric_metrics)
        expect_a_non_empty_data.frame(metrics[[metric]])
})

# all known metrics -------------------------------------------------------
test_that("returns all known class metrics in one command", {
    attach(test_env)

    expect_silent(metrics <- Yardstick$new(data = data_cla, truth = "Species", estimate = "Species"))
    expect_a_non_empty_data.frame(metrics$all_class_metrics)
    expect_setequal(metrics$all_class_metrics$.metric, class_metrics)
})

test_that("returns all known numeric metrics in one command", {
    attach(test_env)

    expect_silent(metrics <- Yardstick$new(data = data_reg, truth = "mpg", estimate = "mpg"))
    expect_a_non_empty_data.frame(metrics$all_numeric_metrics)
    expect_setequal(metrics$all_numeric_metrics$.metric, numeric_metrics)
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
