#' Step: Install Local Package
#'
#' Install the local package
#'
#' @family steps
#' @export
InstallLocal <- R6::R6Class(
    "InstallLocal", inherit = TicStep,

    public = list(

        run = function() {
            message("\n", rep("#",40), "\n", "## Installing the Current Package Version\n", rep("#",40))

            try(utils::remove.packages(private$package_name), silent = TRUE)

            devtools::document()

            devtools::install_local(
                path = ".",
                dependencies = TRUE,
                upgrade = FALSE,
                force = FALSE,
                build = FALSE,
                build_opts = "--no-multiarch --with-keep.source --no-build-vignettes",
                Ncpus = parallel::detectCores(),
                repos = "https://cloud.r-project.org"
            )
        }
    ),
    private = list(
        package_name = desc::description$new()$get_field("Package")
    )
)

step_install_local_package <- function(){
    InstallLocal$new()
}
