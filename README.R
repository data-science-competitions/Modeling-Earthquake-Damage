# High-level Functions ----------------------------------------------------
badge_custom <- function(x, y, color, url = "about::blank"){
    alt <- paste(x)
    x <- gsub(" ", "%20", x)
    y <- gsub(" ", "%20", y)
    x <- gsub("-", "--", x)
    y <- gsub("-", "--", y)
    badge <- paste0("![", alt, "](https://img.shields.io/badge/", x, "-",
                    y, "-", color, ".svg)")
    if (is.null(url))
        return(badge)
    paste0("[", badge, "](", url, ")")
}

plot_function_dependencies <- function(package_name){
    suppressMessages(
        devtools::install_github(
            "datastorm-open/DependenciesGraphs",
            dependencies = TRUE, upgrade = "never")
    )
    suppressPackageStartupMessages(library(DependenciesGraphs))

    package_name %>%
        .get_package_env_name() %>%
        DependenciesGraphs::envirDependencies() %>%
        plot()

}

plot_datasets_overview <- function(x){
    .install_package("DiagrammeR")

    suppressWarnings(
        suppressMessages(
            devtools::install_github(
                "krlmlr/dm",
                dependencies = TRUE, upgrade = "never")
        )
    )

    x %>%
        dm::as_dm() %>%
        dm::cdm_draw(
            col_attr = c("column", "type")[1:2],
            view_type = "all", columnArrows = FALSE, rankdir = "BT"
        )
}

# Low-level Functions ----------------------------------------------------------
.install_package <- function(pkg){
    if(.is_package_installed(pkg)) return(invisible())

    message("--> Installing {", pkg, "}")
    utils::install.packages(pkg,
                            repos = "https://cloud.r-project.org",
                            dependencies = TRUE,
                            Ncpus = parallel::detectCores()
    )

    return(invisible())
}

.get_package_function_names <- function(package_name){
    X <- utils::lsf.str(pos = which(search() %in% .get_package_env_name(package_name)))
    as.vector(X)[!as.vector(X) %in% c("library.dynam.unload", "system.file")]
}

.get_package_env_name <- function(package_name){
    paste0("package:", package_name)
}

.is_package_installed <- function(pkg) pkg %in% rownames(utils::installed.packages())

.get_R_version_dependency <- function(){
    R_details <- description$new()$get_field("Depends")
    R_details <- R_details[grep("^R \\(>=", R_details)]
    gsub("R |[\\(>= \\)]", "", R_details)
}
