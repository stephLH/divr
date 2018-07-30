#' Get package dependancies.
#'
#' @param package_path Package path.
#'
#' @return A dependencies vector.
#'
#' @export
package_deps <- function(package_path = getwd()) {

  deps <- list.files(package_path, pattern = "\\.(R|Rmd)$", full.names = TRUE, recursive = TRUE) %>%
    lapply(readr::read_lines) %>%
    unlist() %>%
    stringr::str_match_all("([[:alnum:]\\.]+)::") %>%
    purrr::map_df(dplyr::as_tibble) %>%
    dplyr::pull(V2) %>% unique() %>% sort() %>%
    .[which(is.na(stringr::str_detect(., stringr::str_match(getwd(), .)[, 1])))]

  return(deps)
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

#' Look for code inside R or Rmd scripts.
#'
#' @param code R syntax to search.
#' @param path Path where R and Rmd files are located.
#'
#' @return A data frame with three fields : file, line number and code containing researched syntax.
#'
#' @export
code_search <- function(code, path = getwd()) {

  fichiers <- list.files(path, recursive = TRUE, pattern = "\\.(R|Rmd)$", full.names = TRUE)

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

#' Look and replace code inside R or Rmd scripts.
#'
#' @param code R syntax to search and replace.
#' @param replacement A character of replacement.
#' @param path Path where R and Rmd files are located.
#'
#' @export
code_replace <- function(code, replacement, path) {

  fichiers <- divr::rechercher_code(code, path) %>%
    dplyr::pull(fichier) %>%
    unique()

  import_code <- purrr::map(fichiers, readr::read_lines) %>%
    purrr::map(stringr::str_replace_all, code, replacement)

  purrr::walk2(import_code, fichiers, readr::write_lines)
}
