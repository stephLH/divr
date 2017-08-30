#' Obtenir les dependances d'un packages
#'
#' Obtenir les dépendances d'un packages.
#'
#' @param repertoire_package Le répertoire racine du package.
#'
#' @return Un vecteur de dépendances.
#'
#' @export
dependances_package <- function(repertoire_package = ".") {

  dependances <- list.files(repertoire_package, pattern = "\\.R$", full.names = TRUE, recursive = TRUE) %>%
    lapply(readr::read_lines) %>%
    unlist() %>%
    stringr::str_match_all("([[:alnum:]\\.]+)::") %>%
    purrr::map_df(dplyr::as_tibble) %>%
    dplyr::pull(V2) %>% unique() %>% sort() %>%
    .[which(is.na(stringr::str_detect(., stringr::str_match(getwd(), .)[, 1])))]

  return(dependances)
}
