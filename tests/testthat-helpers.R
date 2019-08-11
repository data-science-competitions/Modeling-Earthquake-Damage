# Expectations -----------------------------------------------------------------
expect_dir_exists_and_not_empty <- function(path){
    expect_dir_exists(path)
    expect_gte(length(list.files(path, pattern = ".*.R$")), 1)
}
expect_dir_exists <- function(path) expect_true(dir.exists(path))
expect_dir_does_not_exist <- function(path) expect_false(dir.exists(path))
expect_file_exists <- function(path) expect_true(file.exists(path))
expect_no_md_files <- function(path) expect_length(list.files(path, recursive = TRUE, all.files = TRUE, pattern = ".*.md"), 0)
expect_text_appears_in_document <- function(target, text) expect_true(any(grepl(text, readLines(target))))
expect_subset <- function(x, y) expect_true(.is_subset(x ,y))
expect_disjoint_sets <- function(x, y) expect_true(.are_disjoint_sets(x, y))
expect_equal_sets <- function(x, y) expect_true(.are_set_equal(x, y), label = "Sets are not equal")
expect_class <- function(object, class) expect_true(any(base::class(object) %in% class), label = paste("object is a", base::class(object), "not", class))
expect_no_duplicates <- function(x) expect_true(.has_no_duplicates(x))
expect_an_empty_data.frame <- function(x){expect_class(x, "data.frame"); expect_equal(nrow(x), 0, label = paste("data.frame is not-empty; "))}
expect_a_non_empty_data.frame <- function(x){expect_class(x, "data.frame"); expect_gt(nrow(x), 0, label = paste("data.frame is empty; "))}
expect_table_has_col_names <- function(object, col_names) expect_subset(col_names, colnames(object))
expect_not_identical <- function(object, expected) expect_false(identical(object, expected), info  = "Error: objects A and B are identical")

# Predicates -------------------------------------------------------------------
.are_set_equal <- function(x, y){
    return(setequal(x %>% distinct(), y %>% distinct()))
}

.are_disjoint_sets <- function(x, y){
    return(length(intersect(x, y)) == 0)
}

.is_subset <- function(x, y){
    return(length(setdiff(x, y)) == 0)
}

.is_testing <- function(){
    identical(Sys.getenv("TESTTHAT"), "true")
}

.is_developing <- function(){
    identical(Sys.getenv("DEVTOOLS_LOAD"), "true")
}

.is_integrating <- function(){
    identical(Sys.getenv("CI"), "true")
}

.is_not_on_cran <- function(){
    identical(Sys.getenv("NOT_CRAN"), "true")
}

.any_testthat_failures <- function(test_result){
    invisible(capture.output(failure_flag <- print(test_result)$failed > 0))
    return(failure_flag)
}

# Setup and Teardown -----------------------------------------------------------
.create_temp_folder <- function() dir.create(.get_temp_dir(), showWarnings = FALSE, recursive = TRUE)
.delete_temp_folder <- function() unlink(.get_temp_dir(), recursive = TRUE, force = TRUE)

# testthat high-level functions ------------------------------------------------
.setup <- function(){
    try(.save_all(.get_projet_dir()), silent = TRUE)
    try(library(.get_package_name(), character.only = TRUE))
    if(.is_testing()) return(invisible())
    if(.is_developing()) return(invisible())

    try({
        sink(tempfile())
        suppressMessages(devtools::document())
        suppressMessages(devtools::load_all(export_all = FALSE, helpers = FALSE))
        sink()
    })
    invisible()
}

.run_unit_tests <- function(){
    .title("Running Unit Tests")
    testthat::test_dir(file.path(.get_projet_dir(), "tests", "testthat"))
}

.run_component_tests <- function(){
    .title("Running Component Tests")
    testthat::test_dir(file.path(.get_projet_dir(), "tests", "component-tests"))
}

.run_integration_tests <- function(){
    .title("Running Integration Tests")
    testthat::test_dir(file.path(.get_projet_dir(), "tests", "integration-tests"))
}

.run_coverage_tests <- function(){
    if(.is_testing()) return(invisible())
    if(.is_developing()) return(invisible())
    .title("Running Coverage Tests")
    test_result <- testthat::test_dir(file.path(.get_projet_dir(), "tests", "coverage-tests"))
    if(.any_testthat_failures(test_result)){
        .title("Rendering Coverage Report")
        .run_report()
    }
}

.run_report <- function(){
    sink(tempfile())
    try(suppressMessages(devtools::document(pkg = .get_projet_dir())), silent = TRUE)
    try(suppressMessages(devtools::load_all(path = .get_projet_dir(), export_all = FALSE, helpers = FALSE)), silent = TRUE)
    sink()

    Sys.setenv(TESTTHAT = "true")
    on.exit(Sys.setenv(TESTTHAT = ""))
    covr::report()
}

.cleanup <- function(){
    path_temp <- file.path(.get_projet_dir(), 'temp')
    unlink(path_temp, recursive = TRUE, force = TRUE)
}

# testthat low-level functions -------------------------------------------------
.library <- function(package){
    suppressWarnings({
        if(!require(package, character.only = TRUE)){
            message("--> Installing {", package, "}")
            utils::install.packages(package,
                                    repos = "https://cloud.r-project.org",
                                    dependencies = TRUE,
                                    Ncpus = parallel::detectCores()
            )
        }
    })

    library(package, character.only = TRUE)
}

.save_all <- function()
{
    command <- '
    if (rstudioapi::hasFun("documentSaveAll")) {
        rstudioapi::documentSaveAll()
    }'
    eval(parse(text=command))
}

.title <- function(string){
    seperator <- paste0(rep("#", 80), collapse = "")
    message(paste0(c(seperator, paste("##", string), seperator), collapse = "\n"))
}

# Misc -------------------------------------------------------------------------
.get_package_name <- function(){
    is_not_null <- function(x) !is.null(x)

    get_package_name_via_desc <- function(){
        package_name <- try(desc::description$new()$get_field("Package"), silent = TRUE)
        if("try-error" %in% class(package_name)) return(NULL)
        return(package_name)
    }

    get_package_name_via_devtools <- function(){
        package_name <- try(devtools::dev_packages(), silent = TRUE)
        if("try-error" %in% class(package_name)) return(NULL)
        if(length(package_name) == 0) return(NULL)
        return(package_name)
    }

    get_package_name_via_path <- function(){
        basename(.get_projet_dir())
    }

    if(is_not_null(get_package_name_via_desc())){
        return(get_package_name_via_desc())
    } else if (is_not_null(get_package_name_via_devtools())){
        return(get_package_name_via_devtools())
    } else {
        return(get_package_name_via_path())
    }
}

.delete_and_create_dir <- function(path){
    stopifnot(path != .get_projet_dir())
    unlink(path, recursive = TRUE)
    dir.create(path, showWarnings = FALSE, recursive = TRUE)
    return(invisible())
}

.get_temp_dir <- function(){
    proj_path <- .get_projet_dir()
    path_temp <- file.path(proj_path, "temp")
    return(path_temp)
}

.get_projet_dir <- function(){
    proj_path <- getwd()
    while (length(grep("test", proj_path))>0) proj_path <- dirname(proj_path)
    return(proj_path)
}
