#' Obtenir le nombre de mois ecoules entre deux dates
#'
#' Obtenir le nombre de mois écoulés entre deux dates.
#'
#' @param date_debut Date de début de la période
#' @param date_fin Date de fin de la période
#' @param methode Méthode de calcul:\cr
#' arrondi: Un arrondi au plus proche selon le nombre de jours (méthode par défaut).\cr
#' max: Un seul jour dans un mois suffit à le comptabiliser.
#'
#' @return Le nombre de mois écoulés entre les deux dates.\cr
#'
#' @examples
#' # Une année écoulée au jour près
#' divr::mois_ecoules(lubridate::dmy("01/01/2000"), lubridate::dmy("01/01/2001"))
#'
#' # Une année écoulée moins un jour
#' divr::mois_ecoules(lubridate::dmy("01/01/2000"), lubridate::dmy("31/12/2000"))
#'
#' @export
mois_ecoules <- function(date_debut, date_fin, methode = "arrondi") {

  if (methode == "max") {
    mois_ecoules <- 12 * (lubridate::year(date_fin) - lubridate::year(date_debut)) + (lubridate::month(date_fin) - lubridate::month(date_debut)) + 1
  } else if (methode == "arrondi") {
    jour_ecoules <- date_fin - date_debut
    jour_ecoules <- lubridate::days(jour_ecoules)
    mois_ecoules <- jour_ecoules$day / 30
    mois_ecoules <- round(mois_ecoules)
  }

  return(mois_ecoules)
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

#' Obtenir la date du jour
#'
#' Obtenir la date du jour.
#'
#' @param format Le format de la date à retourner (datage_fichier, par défaut | litteral).
#'
#' @return La date du jour.
#'
#' @examples
#' divr::date_jour_fichier()
#'
#' @export
date_jour <- function(format = "datage_fichier") {

  date_jour <- Sys.Date()

  if (format == "datage_fichier") {
    date_jour <- date_jour %>%
      as.character() %>%
      stringr::str_replace_all("-", "_")
  }

  if (format == "litteral") {
    date_jour <- date_jour %>%
      format("%d %B %Y")
  }

  return(date_jour)
}

#' Obtenir un age moyen sous forme litterale
#'
#' Obtenir un âge moyen sous forme littérale.
#'
#' @param format l'âge moyen au format numérique.
#'
#' @return l'âge moyen au format littéral.
#'
#' @examples
#' divr::age_moyen_literral(25.64048)
#'
#' @export
age_moyen_litteral <- function(age_moyen) {

  mois <- age_moyen %>%
    { . - floor(.) } %>%
    { . * 12 } %>%
    round()

  mois <- dplyr::case_when(
    mois >= 1 ~ "mois",
    TRUE ~ NA_character_
  ) %>%
    caractr::paste2(mois, .)

  annee <- floor(age_moyen)

  annee <- dplyr::case_when(
    annee == 1 ~ "an",
    annee >= 2 ~ "ans",
    TRUE ~ NA_character_
    ) %>%
    caractr::paste2(annee, .)

  age_moyen_litteral <- caractr::paste2(annee, mois, sep = " et ")

  return(age_moyen_litteral)
}
