context("unit test for utils-tidyverse")

# flag_na -----------------------------------------------------------------
test_that("flag_na handles data.frame without NA", {
    expect_identical(flag_na(mtcars), mtcars)
})

test_that("flag_na handles data.frame with NA", {
    before_mtcars <- after_mtcars <- mtcars
    before_mtcars[1:4, 1] <- NA
    after_mtcars <- cbind(mpg = before_mtcars[, 1], mpg_NA = as.logical(c(rep(TRUE, 4), rep(FALSE, 28))), before_mtcars[, -1])

    expect_identical(flag_na(before_mtcars), after_mtcars)
})

test_that("flag_na handles data.frame with NA and pre-existing NA indicator column", {
    before_mtcars <- after_mtcars <- mtcars
    before_mtcars[1:4, 1] <- NA
    after_mtcars <- cbind(mpg = before_mtcars[, 1], mpg_NA = as.logical(c(rep(TRUE, 4), rep(FALSE, 28))), before_mtcars[, -1])

    expect_identical(flag_na(after_mtcars), after_mtcars)
})

# treat_na ----------------------------------------------------------------
test_that("treat_na handles data.frame without NA", {
    expect_identical(treat_na(mtcars), mtcars)
})

test_that("treat_na handles data.frame with NA", {
    before_mtcars <- after_mtcars <- mtcars
    before_mtcars[1:4, 1] <- NA
    replace <- list("mpg" = median(before_mtcars$mpg, na.rm = TRUE))
    after_mtcars <- cbind(mpg = before_mtcars[, 1], mpg_NA = as.logical(c(rep(TRUE, 4), rep(FALSE, 28))), before_mtcars[, -1])
    after_mtcars[1:4, 1] <- replace$mpg

    expect_identical(treat_na(before_mtcars, replace), after_mtcars)
})

test_that("treat_na refuses to accept invalid replacement values", {
    replace <- list(mpg = NA)
    expect_error(treat_na(mtcars, replace))
})

# treat_non_finite --------------------------------------------------------
test_that("treat_non_finite handles data.frame without non finite values", {
    expect_identical(treat_non_finite(mtcars), mtcars)
})

test_that("treat_non_finite handles data.frame with non-finite values in numeric columns", {
    before_mtcars <- after_mtcars <- mtcars
    before_mtcars[01:04, 1] <- NA
    before_mtcars[05:08, 1] <- NaN
    before_mtcars[09:12, 1] <- +Inf
    before_mtcars[13:16, 1] <- -Inf

    replace = list("mpg" = 0)
    after_mtcars <- cbind(mpg = before_mtcars[, 1], mpg_NA = as.logical(c(rep(TRUE, 16), rep(FALSE, 16))), before_mtcars[, -1])
    after_mtcars[1:16, 1] <- replace$mpg

    expect_identical(treat_non_finite(before_mtcars, replace), after_mtcars)
})

test_that("treat_non_finite handles data.frame with non-finite values in factor columns", {
    before_mtcars <- after_mtcars <- mtcars
    before_mtcars[01:04, 1] <- NA
    before_mtcars[05:08, 1] <- NaN
    before_mtcars[09:12, 1] <- +Inf
    before_mtcars[13:16, 1] <- -Inf
    before_mtcars$mpg <- as.factor(before_mtcars$mpg)

    replace = list("mpg" = 0)
    after_mtcars <- cbind(mpg = before_mtcars[, 1], mpg_NA = as.logical(c(rep(TRUE, 4), rep(FALSE, 28))), before_mtcars[, -1])
    suppressWarnings(after_mtcars[1:4, 1] <- replace$mpg)

    expect_identical(treat_non_finite(before_mtcars, replace), after_mtcars)
})

test_that("treat_non_finite handles data.frame with non-finite values in logical columns", {
    before_mtcars <- after_mtcars <- mtcars
    before_mtcars[01:04, 1] <- NA
    before_mtcars[05:08, 1] <- NaN
    before_mtcars[09:12, 1] <- +Inf
    before_mtcars[13:16, 1] <- -Inf
    before_mtcars$mpg <- as.logical(before_mtcars$mpg)

    replace <- list("mpg" = TRUE)
    after_mtcars <- cbind(mpg = before_mtcars[, 1], mpg_NA = as.logical(c(rep(TRUE, 8), rep(FALSE, 24))), before_mtcars[, -1])
    after_mtcars[1:8, 1] <- replace$mpg

    expect_identical(treat_non_finite(before_mtcars, replace), after_mtcars)
})

test_that("treat_non_finite refuses to accept invalid replacement values", {
    replace <- list("mpg" = NA)
    expect_error(treat_non_finite(mtcars, replace))

    replace <- list("mpg" = NaN)
    expect_error(treat_non_finite(mtcars, replace))

    replace <- list("mpg" = +Inf)
    expect_error(treat_non_finite(mtcars, replace))

    replace <- list("mpg" = -Inf)
    expect_error(treat_non_finite(mtcars, replace))
})
