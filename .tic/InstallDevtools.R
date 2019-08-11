#' Step: Install R Package Development Tools
#'
#' Install the local package dependencies
#'
#' @family steps
#' @export
InstallDevtools <- R6::R6Class(
    "InstallDevtools", inherit = TicStep,
    public = list(
        run = function() {
            message("\n", rep("#",40), "\n", "## Installing R Package Development Tools\n", rep("#",40))
            invisible(sapply(list.files("./.tic", full.names = TRUE), source, local = environment()))
            install_package("desc")
            install_package("devtools")
            install_package("roxygen2")
            install_package("testthat")
            install_package("covr")
            install_package("karthik/holepunch")
        }
    )
)

step_install_development_tools <- function(){
    InstallDevtools$new()
}
