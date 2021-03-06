context("unit test for Yardstick")

# Setup -------------------------------------------------------------------
testthat::setup({
    assign("test_env", testthat::test_env(), envir = parent.frame())
    set.seed(1001)

    test_env$class_metrics <- c("accuracy", "bal_accuracy", "f_meas")
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

# grouping variables ------------------------------------------------------
test_that("Yardstick reports group variables", {
    attach(test_env)
    data_reg <- data_reg %>% dplyr::group_by(cyl)

    expect_silent(metrics <- Yardstick$new(data = data_reg, truth = "mpg", estimate = "mpg_hat"))
    expect_table_has_col_names(metrics$rmse, "cyl")
})

# multiclass estimator ----------------------------------------------------
test_that("Yardstick allows to set multiclass estimator", {
    attach(test_env)

    expect_silent({
        metrics <- Yardstick$new(data = data_cla, truth = "Species", estimate = "Species")
        results <- metrics$bal_accuracy
    })
    expect_true("macro" %in% results$.estimator)

    expect_silent({
        metrics$set_estimator(value = "micro")
        results <- metrics$bal_accuracy
    })
    expect_true("micro" %in% results$.estimator)
})

# multiclass breakdown ----------------------------------------------------
test_that("Yardstick breaks down multiclass target variables", {
    attach(test_env)
    set.seed(2102)
    data <- iris %>% tibble::add_column(Shuffled_Species = sample(iris$Species))
    expect_silent({
        metrics <- Yardstick$new(data = data, truth = "Species", estimate = "Shuffled_Species")
        results <- metrics$bal_accuracy
    })

    expect_table_has_col_names(results, ".class")
    expect_nrow(results, 4)
    expect_subset(results$.class[-1], levels(iris$Species))
    expect_equal(results$.n, c(150, 50, 50, 50))
    expect_true(results[1, ".estimate"] != results[2, ".estimate"])
})

test_that("Yardstick breaks down non-zero-variance multiclass target variables", {
    attach(test_env)
    data <- iris %>% tibble::add_column(NZV_Species = iris[1, "Species"])
    expect_silent({
        metrics <- Yardstick$new(data = data, truth = "Species", estimate = "NZV_Species")
        results <- metrics$bal_accuracy
    })

    expect_table_has_col_names(results, ".class")
    expect_nrow(results, 4)
    expect_subset(results$.class[-1], levels(iris$Species))
    expect_equal(results$.n, c(150, 50, 50, 50))
})

# sample size -------------------------------------------------------------
test_that("Yardstick reports sample size", {
    attach(test_env)
    data_reg <- data_reg %>% dplyr::group_by(cyl)

    expect_silent(metrics <- Yardstick$new(data = data_reg, truth = "mpg", estimate = "mpg_hat"))
    expect_table_has_col_names(metrics$rmse, ".n")
    expect_nrow(metrics$rmse, 3)
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
    expect_disjoint_sets(colnames(results), c(".metric"))
})

# set estimator -----------------------------------------------------------
test_that("Yardstick allows to set multiclass estimator", {
    attach(test_env)

    expect_silent(metrics <- Yardstick$new(data = data_reg, truth = "mpg", estimate = "mpg_hat"))
    expect_silent(metrics$set_estimator(value = "micro"))
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
