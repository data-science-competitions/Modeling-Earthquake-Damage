# enable_parallelism -----------------------------------------------------
#nocov start
#' @title Create a Parallel Socket Cluster
#'
#' @export
#'
#' @family parallel utility functions
#'
enable_parallelism <- function() {#nocov
    if(is.null(getOption("parallel.cluster"))){
        closeAllConnections()
        options(parallel.enable = TRUE)
        options(parallel.ncores = parallel::detectCores())
        options(parallel.cluster = parallel::makePSOCKcluster(getOption("parallel.ncores")))
        parallel::setDefaultCluster(cl = getOption("parallel.cluster"))
    }
    message("\033[42msocket cluster with ", getOption("parallel.ncores"), " workers is online\033[49m")
    invisible()
}
#nocov end
