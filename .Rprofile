.First <- function(){
    # Helper Functions --------------------------------------------------------
    copy_CONFIGURATION_from_root_to_inst <- function(){
        source <- "CONFIGURATION"
        target <- file.path("inst", "CONFIGURATION")
        dir.create(dirname(target), showWarnings = FALSE, recursive = TRUE)
        file.copy(from = source, to = target, overwrite = TRUE)
    }

    # Main --------------------------------------------------------------------
    sink(tempfile())
    suppressMessages(devtools::load_all(export_all = FALSE, helpers = FALSE))
    sink()

    config::get(file = file.path(rprojroot::find_rstudio_root_file(), "CONFIGURATION"))
    copy_CONFIGURATION_from_root_to_inst()
}

.Last <- function(){
    try({
        deps <- desc::description$new()$get_deps() %>% dplyr::arrange(type, package)
        desc::description$new()$del_deps()$set_deps(deps)$write()
    }, silent = TRUE)
}
