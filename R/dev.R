#' Obtenir les dependances d'un packages
#'
#' Obtenir les dépendances d'un packages.
#'
#' @param repertoire_package Le répertoire racine du package.
#'
#' @return Un vecteur de dépendances.
#'
#' @export
dependances_package <- function(repertoire_package = getwd()) {

  dependances <- list.files(paste0(repertoire_package, "/R"), full.names = TRUE, recursive = TRUE) %>%
    lapply(readr::read_lines) %>%
    unlist() %>%
    stringr::str_match_all("([[:alnum:]\\.]+)::") %>%
    purrr::map_df(dplyr::as_tibble) %>%
    dplyr::pull(V2) %>% unique() %>% sort() %>%
    .[which(is.na(stringr::str_detect(., stringr::str_match(getwd(), .)[, 1])))]

  return(dependances)
}

#' Construire un package
#'
#' Construire un package.
#'
#' @param package Le nom du package.
#' @param documentation Génération de la documentation, \code{TRUE} par défaut.
#'
#' @export
construction_package <- function(package, documentation = TRUE) {

  if (documentation == TRUE) {
    devtools::document(paste0(racine_packages, package), roclets = c('rd', 'collate', 'namespace'))
  }

  zip <- devtools::build(paste0(racine_packages, package), binary = TRUE, args = "--no-multiarch --with-keep.source")
  if (file.exists(zip)) {
    file.remove(zip) %>% invisible()
  }

  .rs.restartR()

}
