ci_get_job_name <- function(){
    tolower(paste0(Sys.getenv("TRAVIS_JOB_NAME"), Sys.getenv("APPVEYOR_JOB_NAME")))
}

show_error_log <- function(){
    if(is_travis())
        print("/home/travis/build/tidylab/boilerplate/boilerplate.Rcheck/00check.log")
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
