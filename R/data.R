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
#' data_frame <- dplyr::tibble(cle1 = c("A", "A", "B", "B"), cle2 = c("1", "1", "2", "3"), champ = 1:4)
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

  if (length(cible) == 1) {
    cible <- rep(cible, length(courant))
    remplacement <- "na_courant"
  }

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

#' remplacer_si
#'
#' @param courant \dots
#' @param cible \dots
#' @param condition \dots
#' @param na_cible \dots
#'
#' @export
remplacer_si <- function(courant, cible, condition = TRUE, na_cible = FALSE){

  if (length(cible) == 1) {
    cible <- rep(cible, length(courant))
  }

  if (length(courant) != length(cible)) {
    stop(paste0("Les vecteur courant et cible doivent être de même taille."), call. = FALSE)
  } else if (class(courant) != class(cible)) {
    stop(paste0("Les vecteur courant et cible doivent être de même classe"), call. = FALSE)
  }

  if (length(condition) == 1 & length(courant) >= 2) {
    if (condition == TRUE) {
      condition <- rep(TRUE, length(courant))
    }
  }

  if (na_cible == FALSE) {
    condition <- condition & !is.na(cible)
  }

  courant <- ifelse(condition, cible, courant)


  return(courant)
}

#' maj_champ
#'
#' @param table \dots
#' @param table_pivot \dots
#' @param champ_maj \dots
#' @param doublons \dots
#'
#' @export
maj_champ <- function(table, table_pivot, champ_maj, ..., doublons = TRUE) {

  cle_noms <- dplyr::quos(...) %>%
    purrr::map_chr(dplyr::quo_name)

  champ_maj <- dplyr::enquo(champ_maj)
  nom_champ_maj <- dplyr::quo_name(champ_maj)

  table_pivot <- table_pivot %>%
    dplyr::select(cle_noms, .champ_maj = !!champ_maj)

  if (doublons == FALSE) {
    table_pivot <- table_pivot %>%
      dplyr::anti_join(divr::doublons(table_pivot, ...), by = cle_noms)
  }

  maj_champ <- table %>%
    dplyr::filter(is.na(!!champ_maj)) %>%
    dplyr::left_join(table_pivot, by = cle_noms) %>%
    dplyr::mutate(!!nom_champ_maj := divr::remplacer_na(!!champ_maj, .champ_maj, remplacement = "na_courant")) %>%
    dplyr::select(-.champ_maj)

  ajout <- tidyr::drop_na(table, !!champ_maj)

  maj_champ <- dplyr::bind_rows(maj_champ, ajout)

  return(maj_champ)
}

#' anti_join_bind
#'
#' @param x Table pivot
#' @param y Table tierce
#' @param by \dots
#' @param arrange \dots
#'
#' @export
anti_join_bind <- function(x, y, by, arrange = TRUE) {

  anti_join_bind <- y %>%
    dplyr::anti_join(x, by) %>%
    dplyr::bind_rows(x, .)

  if (arrange == TRUE) {
    anti_join_bind <- dplyr::arrange(anti_join_bind, !!!rlang::parse_quosure(by))
  }

  return(anti_join_bind)
}
