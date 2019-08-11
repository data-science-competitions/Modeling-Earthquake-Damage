#' Step: Run Test-Suite
#'
#' Run one of "testthat", "component-tests", "integration-tests" or
#' "coverage-tests".
#'
#' @family steps
#' @export
TestSuite <- R6::R6Class(
    "TestSuite", inherit = TicStep,

    public = list(

        initialize = function(job_name){
            private$job_name <- job_name
        },

        run = function() {
            if(private$is_job_name_known(private$job_name) == FALSE) return(invisible())
            message("\n", rep("#",40), "\n", "## Test Suite: ",  private$job_name, "\n", rep("#",40))
            library(private$package_name, character.only = TRUE)
            testthat::test_dir(private$get_path_to_tests(),
                               show_report = TRUE,
                               stop_on_failure = TRUE,
                               package = private$package_name)
        }
    ),
    private = list(
        is_job_name_known = function(job_name){
            job_name %in% list.dirs(file.path(getwd(), "tests"), full.names = FALSE, recursive = FALSE)
        },
        get_path_to_tests = function(){
            file.path(getwd(), "tests", private$job_name)
        },
        job_name = character(),
        package_name = desc::description$new()$get_field("Package")
    )
)

step_run_test_suite <- function(job_name){
    TestSuite$new(job_name)
}
