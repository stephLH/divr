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

#' Stopper un cluster (traitement parallelise)
#'
#' Stopper un cluster (traitement parallélisé)
#'
#' @param cluster
#'
#' @export
#'
#' @examples
#' #' # Créer le cluster:
#' cluster <- divr::initialise_cluster()
#'
#' # Un traitement parallélisé
#'
#' # A l'issue du traitement parallélisé, libérer les coeurs:
#' divr::stop_cluster(cluster)
#'
stopper_cluster <- function(cluster) {
  parallel::stopCluster(cluster)
}
