.First <- function(){
    # Helper Functions --------------------------------------------------------
    copy_CONFIGURATION_from_root_to_inst <- function(){
        source <- "CONFIGURATION"
        target <- file.path("inst", "CONFIGURATION")
        dir.create(dirname(target), showWarnings = FALSE, recursive = TRUE)
        file.copy(from = source, to = target, overwrite = TRUE)
    }

    # Main --------------------------------------------------------------------
    try(config::get(file = file.path(rprojroot::find_rstudio_root_file(), "CONFIGURATION")))
    try(copy_CONFIGURATION_from_root_to_inst())
    try({ # The expectation is needed when using CI
        sink(tempfile())
        on.exit(sink())
        suppressMessages(devtools::load_all(export_all = FALSE, helpers = FALSE))
    })
}

.Last <- function(){
    # Helper Functions --------------------------------------------------------
    is_integrating <- function() identical(Sys.getenv("CI"), "true")
    arrange_DESCRIPTION_requirements_alphabetically <- function(){
        deps <- desc::description$new()$get_deps() %>% dplyr::arrange(type, package)
        desc::description$new()$del_deps()$set_deps(deps)$write()
    }

    # Main --------------------------------------------------------------------
    if(is_integrating()) return()
    try(arrange_DESCRIPTION_requirements_alphabetically(), silent = TRUE)
}

