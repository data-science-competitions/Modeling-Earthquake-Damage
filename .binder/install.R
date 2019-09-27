# Helper Functions --------------------------------------------------------
set_repos_to_MRAN <- function(){
    options(repos = get_MRAN_URL())
}

get_MRAN_URL <- function(){
    MRAN_timestamp <- get_binder_timestamp()
    paste0("https://mran.microsoft.com/snapshot/", MRAN_timestamp)
}

get_binder_timestamp <- function(){
    file_path <- list.files(pattern = "runtime.txt", full.names = TRUE, recursive = TRUE, all.files = TRUE)
    as.Date(gsub("^r-", "", readLines(file_path, 1)))
}

# Install Packages --------------------------------------------------------
set_repos_to_MRAN()
install.packages("devtools")
install.packages("tidyverse", dependencies = TRUE)
try({
    devtools::install_github("data-science-competitions/modeling.earthquake.damage", dependencies = TRUE)
    devtools::uninstall("modeling.earthquake.damage")
})
