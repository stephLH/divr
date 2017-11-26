#' Extraire les doublons d'une table selon un ou plusieurs champs
#'
#' Extraire les doublons d'une table selon un ou plusieurs champs.
#'
#' @param table Data frame dans lequel sont extraits les doublons.
#' @param cle Vecteur de noms de champ au croisement desquels sont extraits les doublons.
#'
#' @return Un extrait du data frame correspondant aux enregistrements doublon.
#'
#' @examples
#' data_frame <- tibble::tibble(cle1 = c("A", "A", "B", "B"), cle2 = c("1", "1", "2", "3"), champ = 1:4)
#'
#' # Un exemple avec des doublons
#' divr::doublons(data_frame, cle1, cle2)
#'
#' # Un exemple sans doublon
#' divr::doublons(data_frame, cle1, cle2, champ))
#'
#' @export
doublons <- function(table, ...){

  if (any(class(table) == "data.frame") == FALSE) {
    stop("Le premier paramètre doit être de type data.frame (ou tibble)", call. = FALSE)
  }

  group_by <- dplyr::quos(...)

  doublons <- dplyr::group_by(table, !!!group_by) %>%
    dplyr::filter(row_number() >= 2) %>%
    dplyr::ungroup() %>%
    dplyr::select(purrr::map_chr(group_by, dplyr::quo_name)) %>%
    unique() %>%
    dplyr::right_join(table, ., by = purrr::map_chr(group_by, dplyr::quo_name))

  return(doublons)
}

#' remplacer_na
#'
#' @param courant \dots
#' @param cible \dots
#' @param remplacement (défaut|na_courant).
#'
#' @export
remplacer_na <- function(courant, cible, remplacement = "défaut"){

  if (length(courant) != length(cible)) {
    stop(paste0("Les vecteur courant et cible doivent être de même taille."), call. = FALSE)
  } else if (class(courant) != class(cible)) {
    stop(paste0("Les vecteur courant et cible doivent être de même classe"), call. = FALSE)
  }

  if (remplacement == "défaut") {
    courant <- ifelse(!is.na(cible), cible, courant)
  } else if (remplacement == "na_courant") {
    courant[which(is.na(courant))] <- cible[which(is.na(courant))]
  }

  return(courant)
}

#' maj_champ
#'
#' @param table \dots
#' @param table_pivot \dots
#' @param champ_maj \dots
#' @param \dots
#'
#' @export
maj_champ <- function(table, table_pivot, champ_maj, ...) {

  cle_noms <- dplyr::quos(...) %>%
    purrr::map_chr(dplyr::quo_name)

  champ_maj <- dplyr::enquo(champ_maj)
  nom_champ_maj <- dplyr::quo_name(champ_maj)

  maj_champ <- table %>%
    dplyr::filter(is.na(!!champ_maj)) %>%
    dplyr::left_join(table_pivot %>%
                       dplyr::select(cle_noms, .champ_maj = !!champ_maj),
                     by = cle_noms) %>%
    dplyr::mutate(!!nom_champ_maj := divr::remplacer_na(!!champ_maj, .champ_maj, remplacement = "na_courant")) %>%
    dplyr::select(-.champ_maj)

  ajout <- tidyr::drop_na(table, !!champ_maj)

  maj_champ <- dplyr::bind_rows(maj_champ, ajout)

  return(maj_champ)
}

