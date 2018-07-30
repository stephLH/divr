#' Initialise cores for a parallel computation.
#'
#' @param n_cores Number of cores to use (maximum by default).
#'
#' @return An objectof type cluster.
#'
#' @export
initialise_cluster <- function(n_cores = NULL){

  if (is.null(n_cores)) {
    n_cores <- parallel::detectCores()
  }

  cluster <- parallel::makeCluster(n_cores)

  doParallel::registerDoParallel(cluster)

  return(cluster)
}

#' Stop a cluster after a parallel computation.
#'
#' @param cluster A cluster object returnes by \code{initialise_cluster}
#'
#' @export
stop_cluster <- function(cluster) {
  parallel::stopCluster(cluster)
}
