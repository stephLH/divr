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
#' data_frame <- dplyr::data_frame(cle1 = c("A", "A", "B", "B"), cle2 = c("1", "1", "2", "3"), champ = 1:4)
#'
#' # Un exemple avec des doublons
#' divr::doublons(data_frame, cle = c("cle1", "cle2"))
#'
#' # Un exemple sans doublon
#' divr::doublons(data_frame, cle = c("cle1", "cle2", "champ"))
#'
#' @export
doublons <- function(table, cle){

  if (any(class(table) == "data.frame") == FALSE) {
    stop("Le premier paramètre doit être de type data.frame (ou tibble)", call. = FALSE)
  }

  num_champ <- which(colnames(table) %in% cle)

  if (length(num_champ) != length(cle)) {
    cle_manquante <- cle[which(cle %in% colnames(table))]
    stop(paste0("Le(les) champ(s) \"", paste(cle, sep = ", ") ,"\" ne sont pas présents dans la table"), call. = FALSE)
  }

  doublons <- dplyr::select(table, !!cle) %>%
    # dplyr::group_by(.data[[cle]]) %>%
    dplyr::group_by_(.dots = cle) %>%
    dplyr::filter(row_number() >= 2) %>%
    dplyr::ungroup() %>%
    #dplyr::select(-cle) %>%
    unique() %>%
    dplyr::right_join(table, ., by = cle)

  return(doublons)
}
