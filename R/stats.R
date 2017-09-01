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

  count_agregats <- dplyr::count(table, !!!lapply(c(noms_champs_cle, noms_champs_count), rlang::parse_quosure))

  ajout_agregat <- function(champ, valeur_total) {

    champs_count <- noms_champs_count[-c(which(noms_champs_count == champ):length(noms_champs_count))]

    champs_mutate <- noms_champs_count[c(which(noms_champs_count == champ):length(noms_champs_count))]
    mutate <- stats::setNames(as.list(rep(NA_character_, length(champs_mutate))),
                              as.list(champs_mutate))

    dplyr::count(table, !!!lapply(c(noms_champs_cle, champs_count), rlang::parse_quosure)) %>%
      dplyr::mutate(!!!mutate) %>%
      dplyr::mutate(!!champ := valeur_total)
  }

  count_agregats <- count_agregats %>%
    dplyr::bind_rows(
      purrr::map_df(noms_champs_count, ajout_agregat, valeur_total = valeur_total)
    )

  return(count_agregats)
}
