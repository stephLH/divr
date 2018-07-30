#' Initialiser les coeurs pour un traitement parallelise
#'
#' Initialiser les coeurs pour un traitement parallélisé.
#'
#' @param n_coeurs Nombre de coeurs à utiliser (le maximum par défaut).
#'
#' @return Un objet de type cluster.\cr
#' Après les calculs parallèles, il convient de libérer le cluster avec la commande \code{parallel::stopCluster()} en le passant en paramètre.
#'
#' @examples
#' # Créer le cluster:
#' cluster <- divr::initialiser_cluster()
#'
#' # Un traitement parallélisé
#'
#' # A l'issue du traitement parallélisé, libérer les coeurs:
#' divr::stopCluster(cluster)
#'
#' @export
initialiser_cluster <- function(n_coeurs = NULL){

  if (is.null(n_coeurs)) {
    n_coeurs <- parallel::detectCores()
  }

  cluster <- parallel::makeCluster(n_coeurs)

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
#' cluster <- divr::initialiser_cluster()
#'
#' # Un traitement parallélisé
#'
#' # A l'issue du traitement parallélisé, libérer les coeurs:
#' divr::stopper_cluster(cluster)
#'
stopper_cluster <- function(cluster) {
  parallel::stopCluster(cluster)
}
