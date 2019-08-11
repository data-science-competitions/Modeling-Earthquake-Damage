ci_get_job_name <- function(){
    tolower(paste0(Sys.getenv("TRAVIS_JOB_NAME"), Sys.getenv("APPVEYOR_JOB_NAME")))
}

show_error_log <- function(){
    `%+%` <- function(a,b) paste0(a, b)

    install_package("desc")
    install_package("stringr")
    install_package("devtools")

    desc_obj <- description$new()
    package_url <- desc_obj$get_field("BugReports") %>% stringr::str_remove("/issues$")
    package_name <- desc_obj$get_field("Package")
    package_repo <- stringr::str_extract_all(package_url, "[^/]+(?://[^/]*)*")[[1]][2:3] %>% paste0(collapse ="/")

    if(is_travis()){
        error_log <- "/home/travis/build/" %+% package_repo %+% "/" %+% package_name %+% ".Rcheck/00check.log"
        try(print(readLines(error_log)), silent = TRUE)
    }

    print(devtools::session_info())
}

is_travis <- function(){
    identical(Sys.getenv("TRAVIS"), "true")
}

install_package <- function(pkg){
    # Helper Functions ---------------------------------------------------------
    get_package_name <- function(pkg) sub("^.*/","", pkg)
    is_package_installed <- function(pkg) pkg %in% rownames(utils::installed.packages())
    install_form_GitHub <- function(pkg){
        message("--> Installing {", get_package_name(pkg), "}")
        install_from_CRAN("devtools")
        devtools::install_github(pkg, dependencies = TRUE, upgrade = "never")
    }
    install_from_CRAN <- function(pkg){
        message("--> Installing {", get_package_name(pkg), "}")
        utils::install.packages(
            pkg,
            repos = "https://cloud.r-project.org",
            dependencies = TRUE,
            Ncpus = parallel::detectCores()
        )
    }

    # Program Logic ------------------------------------------------------------
    if(is_package_installed(get_package_name(pkg))){
        return(invisible())
    } else if (grepl("/", pkg)){
        install_form_GitHub(pkg)
    } else {
        install_from_CRAN(pkg)
    }
    return(invisible())
}
