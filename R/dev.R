#' Obtenir les dependances d'un packages
#'
#' Obtenir les dépendances d'un packages.
#'
#' @param repertoire Le répertoire racine du package.
#'
#' @return Un vecteur de dépendances.
#'
#' @examples
#'
#' @export
dependances_package <- function(repertoire = ".") {

  dependances <- list.files(repertoire, pattern = "\\.R$", full.names = TRUE, recursive = TRUE) %>%
    purrr::map( ~ readr::read_lines(.)) %>%
    unlist() %>%
    stringr::str_match_all("([A-z]+)::") %>%
    purrr::map_df( ~ dplyr::as_tibble(.)) %>%
    dplyr::pull(V2) %>% unique()

  return(dependances)
}
