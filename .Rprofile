.First <- function(){
    # Helper Functions --------------------------------------------------------
    .install_package <- function(pkg){
        is_package_installed <- function(pkg) pkg %in% rownames(utils::installed.packages())

        if(is_package_installed(pkg)) return(invisible())

        message("--> Installing {", pkg, "}")
        utils::install.packages(
            pkg,
            repos = "https://cloud.r-project.org",
            dependencies = TRUE
        )

        return(invisible())
    }

    copy_CONFIGURATION_from_root_to_inst <- function(){
        source <- "CONFIGURATION"
        target <- file.path("inst", "CONFIGURATION")
        dir.create(dirname(target), showWarnings = FALSE, recursive = TRUE)
        file.copy(from = source, to = target, overwrite = TRUE)
    }

    # Main --------------------------------------------------------------------
    try({ # The expectation is needed when using Travis
        .install_package("devtools")
        .install_package("roxygen2")

        sink(tempfile())
        suppressMessages(devtools::document())
        suppressMessages(devtools::load_all(export_all = FALSE, helpers = FALSE))
        sink()

        .install_package("config")
        config::get(file = file.path(rprojroot::find_rstudio_root_file(), "CONFIGURATION"))
        copy_CONFIGURATION_from_root_to_inst()
    }, silent = TRUE)
}

.Last <- function(){
    try({
        deps <- desc::description$new()$get_deps() %>% dplyr::arrange(type, package)
        desc::description$new()$del_deps()$set_deps(deps)$write()
    }, silent = TRUE)
}
