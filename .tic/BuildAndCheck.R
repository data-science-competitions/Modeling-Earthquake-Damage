#' Step: Build and Check Package
#'
#' Build and check package
#'
#' @family steps
#' @export
BuildAndCheck <- R6::R6Class(
    "BuildAndCheck", inherit = TicStep,

    public = list(
        initialize = function(job_name){
            private$job_name <- job_name
        },
        run = function() {
            if(private$is_job_name_known(private$job_name) == FALSE) return(invisible())
            message("\n", rep("#",40), "\n", "## Building and Checking Package\n", rep("#",40))
            system("R CMD build . --no-build-vignettes --no-manual")
            system("R CMD check $(ls -1t *.tar.gz | head -n 1) --no-manual")
        }
    ),
    private = list(
        is_job_name_known = function(job_name) job_name %in% c("build"),
        job_name = character()
    )
)

step_build_and_check <- function(job_name){
    BuildAndCheck$new(job_name)
}
