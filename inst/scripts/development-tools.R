# Functions Dependencies -------------------------------------------------------
devtools::install_github(
    "datastorm-open/DependenciesGraphs",
    force = FALSE, dependencies = TRUE, upgrade = "never")
library(DependenciesGraphs)
devtools::load_all()
## Option 1:
env_name <- paste0("package:", devtools::dev_packages())
dep <- DependenciesGraphs::envirDependencies(env_name)
plot(dep, block = TRUE)
## Option 2:
DependenciesGraphs::launch.app()
