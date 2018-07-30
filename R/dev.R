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

#' Build a package.
#'
#' @param package_path Package path.
#' @param documentation If \code{TRUE}, documentation is also generated.
#'
#' @export
package_build <- function(package_path, documentation = TRUE) {

  if (documentation == TRUE) {
    devtools::document(package_path, roclets = c('rd', 'collate', 'namespace'))
  }

  list.files(R.home(), pattern = "R\\.exe$", recursive = TRUE, full.names = TRUE) %>%
    head(1) %>%
    system2(paste0("CMD INSTALL --no-multiarch --with-keep.source \"", package_path, "\""))

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
code_find <- function(code, path = getwd()) {

  fichiers <- list.files(path, recursive = TRUE, pattern = "\\.(R|Rmd)$", full.names = TRUE)

  code_find <- dplyr::tibble(
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

  return(code_find)
}

#' Look and replace code inside R or Rmd scripts.
#'
#' @param code R syntax to search and replace.
#' @param replacement A character of replacement.
#' @param path Path where R and Rmd files are located.
#'
#' @export
code_replace <- function(code, replacement, path) {

  fichiers <- divr::code_find(code, path) %>%
    dplyr::pull(fichier) %>%
    unique()

  import_code <- purrr::map(fichiers, readr::read_lines) %>%
    purrr::map(stringr::str_replace_all, code, replacement)

  purrr::walk2(import_code, fichiers, readr::write_lines)
}
