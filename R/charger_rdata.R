#' Charger un objet depuis un fichier RData
#'
#' Charger un objet depuis un fichier RData.
#'
#' @param rdata Le chemin vers le fichier RData.
#' @param nom_objet Le nom de l'objet à charger.
#'
#' @return Si le paramètre \code{nom_objet} reste à \code{NULL} alors l'ensemble des objets du fichier RData sont chargés dans une liste nommée.\cr
#' Sinon, seul l'élément passé en paramètre \code{nom_objet} est chargé.
#'
#' @examples
#' # Chargement dans une liste de tous les objets d'un fichier RData (ici, un seul objet nommé "data_lib_num_mois"))
#' divr::charger_rdata(paste0(racine_packages, "divr/data/data_lib_num_mois.RData"))
#'
#' # Chargement d'un objet déterminé
#' divr::charger_rdata(paste0(racine_packages, "divr/data/data_lib_num_mois.RData"), "data_lib_num_mois")
#'
#' @export
charger_rdata <- function(rdata, nom_objet = NULL){

  if (!file.exists(rdata)) {
    stop(paste0("Le fichier \"", rdata, "\" n'existe pas."), call. = FALSE)
  }

  env = new.env()
  load(file = rdata, envir = env)

  if (!is.null(nom_objet)) {
    charger_rdata <- env[[nom_objet]]
  } else {
    charger_rdata <- as.list(env)
  }

  return(charger_rdata)
}
