#' Obtenir le nombre de mois ecoules entre deux dates
#'
#' Obtenir le nombre de mois écoulés entre deux dates.
#'
#' @param date_debut Date de début de la période
#' @param date_fin Date de fin de la période
#'
#' @return Le nombre de mois écoulés entre les deux dates.\cr
#' Un seul jour dans un mois suffit à le comptabiliser.
#'
#' @examples
#' # Une année écoulée au jour près
#' divr::mois_ecoules("2000-01-01", "2001-01-01")
#'
#' # Une année écoulée moins un jour
#' divr::mois_ecoules("2000-01-01", "2000-12-31")
#'
#' @export
mois_ecoules <- function(date_debut, date_fin) {

  date_fin <- as.POSIXlt(date_fin)
  date_debut <- as.POSIXlt(date_debut)

  mois_ecoules <- 12 * (date_fin$year - date_debut$year) + (date_fin$mon - date_debut$mon) + 1

  return(mois_ecoules)
}

#' Obtenir la date du jour (format yyyy_mm_jj) pour prefixer un fichier
#'
#' Obtenir la date du jour (format yyyy_mm_jj) pour préfixer un fichier.
#'
#' @return La date du jour au format yyyy_mm_jj
#'
#' @examples
#' divr::date_jour_fichier()
#'
#' @export
date_jour_fichier <- function() {

  date_jour_fichier <- Sys.Date() %>%
    as.character() %>%
    stringr::str_replace_all("-", "_")

  return(date_jour_fichier)
}

#' Obtenir le numero de mois a partir du libelle
#'
#' Obtenir le numéro de mois à partir du libellé.
#'
#' @param lib_mois Un vecteur de libellés de mois.
#'
#' @return Un vecteur de numéros de mois.
#'
#' Jeu de données source : \code{divr::data_lib_num_mois}.\cr
#' Il est créé à partir de la table "Libelle_num_mois" de la base Access "Tables_ref.accdb" (projet "Temps").
#'
#' @examples
#' divr::conv_lib_num_mois(c("janvier", "Février", "avril"))
#'
#' @export
conv_lib_num_mois <- function(lib_mois) {

  conv_lib_num_mois <- dplyr::data_frame(lib_mois) %>%
    dplyr::mutate(lib_mois = tolower(lib_mois)) %>%
    dplyr::left_join(divr::data_lib_num_mois, by = "lib_mois") %>%
    .[["num_mois"]]

  return(conv_lib_num_mois)
}
