context("unit test for utils-rsample")

test_that("get_rsample_num_of_splits works", {
    ## Setup
    set.seed(139)
    rset_list <- list()
    rset_list[["initial_split"]] <- rsample::initial_split(mtcars)
    rset_list[["vfold_cv"]] <- rsample::vfold_cv(mtcars, v = 5, repeats = 1)
    rset_list[["rvfold_cv"]] <- rsample::vfold_cv(mtcars, v = 5, repeats = 2)
    rset_list[["bootstraps"]] <- rsample::bootstraps(mtcars, times = 25)

    ## Invalid input argument
    expect_error(get_rsample_num_of_splits())
    expect_error(get_rsample_num_of_splits(data.frame(row.names = 1:10)))
    expect_error(get_rsample_num_of_splits(list()))

    ## Valid input argument
    expect_equal(get_rsample_num_of_splits(rset_list[["initial_split"]]), 1)
    expect_equal(get_rsample_num_of_splits(rset_list[["vfold_cv"]]), 5 * 1)
    expect_equal(get_rsample_num_of_splits(rset_list[["rvfold_cv"]]), 5 * 2)
    expect_equal(get_rsample_num_of_splits(rset_list[["bootstraps"]]), 25)
})

test_that("get_rsample_training_set works", {
    ## Setup
    set.seed(139)
    rset_list <- list()
    rset_list[["initial_split"]] <- rsample::initial_split(mtcars)
    rset_list[["vfold_cv"]] <- rsample::vfold_cv(mtcars, v = 5, repeats = 1)
    rset_list[["rvfold_cv"]] <- rsample::vfold_cv(mtcars, v = 5, repeats = 2)
    rset_list[["bootstraps"]] <- rsample::bootstraps(mtcars, times = 25)

    ## Invalid input argument
    ### No input arguments
    expect_error(get_rsample_training_set())
    ### "split" is missing
    expect_error(get_rsample_training_set(rset_list[["initial_split"]]))
    ### "split" is greater than the number of splits in the object
    expect_error(get_rsample_training_set(rset_list[["initial_split"]], 50))
    expect_error(get_rsample_training_set(rset_list[["vfold_cv"]], 50))

    ## Valid input argument
    X_tr <- rsample::training(rset_list[["initial_split"]])
    expect_identical(get_rsample_training_set(rset_list[["initial_split"]], 1), X_tr)
    X_tr <- rsample::training(rset_list[["vfold_cv"]]$splits[[5]])
    expect_identical(get_rsample_training_set(rset_list[["vfold_cv"]], 5), X_tr)
    X_tr <- rsample::training(rset_list[["rvfold_cv"]]$splits[[8]])
    expect_identical(get_rsample_training_set(rset_list[["rvfold_cv"]], 8), X_tr)
    X_tr <- rsample::training(rset_list[["bootstraps"]]$splits[[20]])
    expect_identical(get_rsample_training_set(rset_list[["bootstraps"]], 20), X_tr)
})


test_that("get_rsample_test_set works", {
    ## Setup
    set.seed(139)
    rset_list <- list()
    rset_list[["initial_split"]] <- rsample::initial_split(mtcars)
    rset_list[["vfold_cv"]] <- rsample::vfold_cv(mtcars, v = 5, repeats = 1)
    rset_list[["rvfold_cv"]] <- rsample::vfold_cv(mtcars, v = 5, repeats = 2)
    rset_list[["bootstraps"]] <- rsample::bootstraps(mtcars, times = 25)

    ## Invalid input argument
    ### No input arguments
    expect_error(get_rsample_test_set())
    ### "split" is missing
    expect_error(get_rsample_test_set(rset_list[["initial_split"]]))
    ### "split" is greater than the number of splits in the object
    expect_error(get_rsample_test_set(rset_list[["initial_split"]], 50))
    expect_error(get_rsample_test_set(rset_list[["vfold_cv"]], 50))

    ## Valid input argument
    X_tr <- rsample::testing(rset_list[["initial_split"]])
    expect_identical(get_rsample_test_set(rset_list[["initial_split"]], 1), X_tr)
    X_tr <- rsample::testing(rset_list[["vfold_cv"]]$splits[[5]])
    expect_identical(get_rsample_test_set(rset_list[["vfold_cv"]], 5), X_tr)
    X_tr <- rsample::testing(rset_list[["rvfold_cv"]]$splits[[8]])
    expect_identical(get_rsample_test_set(rset_list[["rvfold_cv"]], 8), X_tr)
    X_tr <- rsample::testing(rset_list[["bootstraps"]]$splits[[20]])
    expect_identical(get_rsample_test_set(rset_list[["bootstraps"]], 20), X_tr)
})

test_that("output is not a tibble", {
    skip_if_not_installed("dplyr")

    set.seed(139)
    rset_list <- list()
    rset_list[["initial_split"]] <- rsample::initial_split(mtcars %>% dplyr::as_tibble())

    expect_not_a_tbl(get_rsample_training_set(rset_list[["initial_split"]], 1))
    expect_not_a_tbl(get_rsample_test_set(rset_list[["initial_split"]], 1))
})
