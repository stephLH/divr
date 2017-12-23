#' count_agregats
#'
#' @param table \dots
#' @param ... \dots
#' @param valeur_total \dots
#'
#' @return
#'
#' @examples
#' tibble::dplyr(var1 = c(1, 1, 2, 2) %>% as.character(), var2 = c(1, 2, 1, 2) %>% as.character(), var3 = c(1, 2, 2, 2) %>% as.character())
#'
#' @export
count_agregats <- function(table, ..., valeur_total = "Total") {

  champs <- dplyr::quos(...)
  noms_champs_count <- purrr::map_chr(dplyr::enquo(champs)[[2]], dplyr::quo_name)
  noms_champs_cle <- names(table)[!names(table) %in% noms_champs_count]

  count_agregat <- function(table, champ, valeur_total) {

    champs_count <- noms_champs_count[-c(which(noms_champs_count == champ):length(noms_champs_count))]

    champs_mutate <- noms_champs_count[c(which(noms_champs_count == champ):length(noms_champs_count))]
    mutate <- stats::setNames(as.list(rep(NA_character_, length(champs_mutate))),
                              as.list(champs_mutate))

    count_agregat <- table %>%
      dplyr::filter(!!!rlang::parse_quosure(paste0("!is.na(", champ,")"))) %>%
      dplyr::count(!!!lapply(c(noms_champs_cle, champs_count), rlang::parse_quosure)) %>%
      dplyr::mutate(!!!mutate) %>%
      dplyr::mutate(!!champ := valeur_total)

    return(count_agregat)
  }

  count_agregats <- table %>%
    dplyr::count(!!!lapply(c(noms_champs_cle, noms_champs_count), rlang::parse_quosure)) %>%
    dplyr::bind_rows(
      purrr::map_df(noms_champs_count, ~ count_agregat(table, ., valeur_total = valeur_total))
    )

  return(count_agregats)
}
