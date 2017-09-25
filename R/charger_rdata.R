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
#' # Chargement dans une liste de tous les objets d'un fichier RData (ici, un seul objet nommé "lib_num_mois"))
#' divr::charger_rdata(paste0(racine_packages, "divr/data/lib_num_mois.RData"))
#'
#' # Chargement d'un objet déterminé
#' divr::charger_rdata(paste0(racine_packages, "divr/data/lib_num_mois.RData"), "lib_num_mois")
#'
#' @export
charger_rdata <- function(chemin_rdata, nom_objet = NULL){

  if (!file.exists(chemin_rdata)) {
    stop(paste0("Le fichier \"", chemin_rdata, "\" n'existe pas."), call. = FALSE)
  }

  env = new.env()
  load(file = chemin_rdata, envir = env)

  fichier_rdata <- stringr::str_match(chemin_rdata, "([^\\/]+?\\.RData)$")[, 2]

  if (length(env) == 1 & paste0(names(env)[1], ".RData") == fichier_rdata) {
    charger_rdata <- env[[names(env)]]
    return(charger_rdata)
  }

  if (!is.null(nom_objet)) {
    charger_rdata <- env[[nom_objet]]
  } else {
    charger_rdata <- as.list(env)
  }

  return(charger_rdata)
}
