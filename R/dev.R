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

#' Rechercher une syntaxe de code dans les fichiers R
#'
#' Rechercher une syntaxe de code dans les fichiers R.
#'
#' @param code Le code R à rechercher.
#' @param chemin Le chemin contenant les programmes R.
#'
#' @return Un tibble à trois champs : le fichier, le numéro de ligne et le code contenant la synatxe recherchée.
#'
#' @export
rechercher_code <- function(code, chemin) {

  fichiers <- list.files(chemin, recursive = TRUE, pattern = "\\.R$", full.names = TRUE)

  rechercher_code <- dplyr::tibble(
    fichier = fichiers,
    code = purrr::map(fichiers, readr::read_table, col_names = "code", col_types = "c")
  ) %>%
    tidyr::unnest() %>%
    dplyr::group_by(fichier) %>%
    dplyr::mutate(ligne = dplyr::row_number()) %>%
    dplyr::ungroup() %>%
    dplyr::select(fichier, ligne, code) %>%
    tidyr::drop_na(code) %>%
    dplyr::filter(stringr::str_detect(code, !!code))

  return(rechercher_code)
}
