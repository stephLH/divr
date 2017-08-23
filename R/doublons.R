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
