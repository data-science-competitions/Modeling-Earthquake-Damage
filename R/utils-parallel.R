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
        options(parallel.enable = TRUE)
        options(parallel.ncores = parallel::detectCores())
        options(parallel.cluster = parallel::makeCluster(getOption("parallel.ncores")))
        parallel::setDefaultCluster(cl = getOption("parallel.cluster"))
    }
    invisible()
}
#nocov end
