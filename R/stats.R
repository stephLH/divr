#' count_agregat
#'
#' @param table \dots
#' @param champ \dots
#' @param valeur_total \dots
#'
#' @return
#'
#' @export
#' @keywords internal
count_agregat <- function(table, champ, valeur_total) {

  champs_count <- noms_champs_count[-c(which(noms_champs_count == champ):length(noms_champs_count))]

  champs_mutate <- noms_champs_count[c(which(noms_champs_count == champ):length(noms_champs_count))]
  mutate <- stats::setNames(as.list(rep(NA_character_, length(champs_mutate))),
                            as.list(champs_mutate))

  count_agregat <- dplyr::count(table, !!!lapply(c(noms_champs_cle, champs_count), rlang::parse_quosure)) %>%
    dplyr::mutate(!!!mutate) %>%
    dplyr::mutate(!!champ := valeur_total)

  return(count_agregat)
}

#' count_agregats
#'
#' @param table \dots
#' @param ... \dots
#' @param valeur_total \dots
#'
#' @return
#'
#' @export
count_agregats <- function(table, ..., valeur_total = "Total") {

  champs <- dplyr::quos(...)
  noms_champs_count <- purrr::map_chr(dplyr::enquo(champs)[[2]], dplyr::quo_name)
  noms_champs_cle <- names(table)[!names(table) %in% noms_champs_count]

  count_agregats <- dplyr::count(table, !!!lapply(c(noms_champs_cle, noms_champs_count), rlang::parse_quosure)) %>%
  dplyr::bind_rows(
      purrr::map_df(noms_champs_count, ~ count_agregat(table, ., valeur_total = valeur_total))
    )

  return(count_agregats)
}