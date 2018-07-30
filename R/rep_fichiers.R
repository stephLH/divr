#' Extract files from a zip.
#'
#' Files within the zip file can be selected with the pattern argument.
#'
#' @param zip_file Path to the zip file.
#' @param pattern an optional regular expression. Only file names which match the regular expression will be extracted.
#' @param exdir The directory to extract files to. It will be created if necessary.
#' @param remove_zip If \code{TRUE}, the zip file is removed.
#'
#' @examples
#' divr::zip_extract_file("archive.zip", pattern = "pdf$", exdir = "test_zip")
#'
#' @export
zip_extract_file <- function(zip_file, pattern = NULL, exdir = NULL, remove_zip = FALSE) {

  if (!file.exists(zip_file)) {
    stop("The zip file \"", zip_file,"\" does not exist", call. = FALSE)
  }

  files <- unzip(zip_file, list = TRUE)[["Name"]]

  if (!is.null(pattern)) files <- files[stringr::str_detect(files, pattern)]

  if (is.null(exdir)) {
    exdir <- stringr::str_match(zip_file, "(.+)/")[, 2]
  }

  unzip(zip_file, files, exdir = exdir)

  if (remove_zip == TRUE) {
    file.remove(zip_file) %>%
      invisible()
  }
}

#' Extraire les fichiers des archives zip d'un repertoire (recursif)
#'
#' Extraire des fichiers des archives zip d'un répertoire (récursif).
#'
#' @param chemin Répertoire dans lequel sont cherchés (récursif) les archives zip dont le contenu est extrait.
#' @param regex_fichier Expression régulière pour filtrer les fichiers csv à extraire.
#' @param regex_zip Expression régulière pour filtrer les archives zip à traiter.
#' @param n_fichiers Nombre de fichiers à importer. Une valeur négative correspond au nombre de fichiers à partir de la fin dans la liste.
#' @param return_tibble Retourne ou non un data frame fournissant la correspondance entre les archives zip initiales et l'emplacement des fichiers extraits.
#' @param paralleliser \code{TRUE}, extraction parallelisée des archives zip.
#'
#' @examples
#' # Extraire les fichiers pdf stockés dans les archives zip du répertoire "Chemin/vers/un/répartoire"
#' divr::extraire_masse_zip("Chemin/vers/un/répartoire", regex_fichier = "pdf$", repertoire_sortie = "test_zip")
#'
#' @export
extraire_masse_zip <- function(chemin, regex_fichier, regex_zip = "\\.zip$", n_fichiers = Inf, return_tibble = TRUE, paralleliser = FALSE) {

  if (!dir.exists(chemin)) {
    stop("Le répertoire \"", chemin,"\" n'existe pas.", call. = FALSE)
  }

  if (paralleliser == TRUE) {
    clusters <- divr::cl_initialise()
  } else {
    clusters <- NULL
  }

  archives_zip <- dplyr::tibble(archive_zip = list.files(chemin, recursive = TRUE, full.names = TRUE) %>%
                                  stringr::str_subset(regex_zip))

  if (nrow(archives_zip) == 0) {
    message("Aucune archive zip dans le répertoire \"", chemin,"\"")
    return(invisible(NULL))
  }

  archives_zip <- archives_zip %>%
    dplyr::mutate(num_archive = dplyr::row_number() %>% as.character())

  archives_zip <- purrr::map_df(archives_zip$archive_zip, unzip, list = TRUE, .id = "num_archive") %>%
    dplyr::select(num_archive, fichier = Name) %>%
    dplyr::filter(stringr::str_detect(fichier, regex_fichier)) %>%
    dplyr::inner_join(archives_zip, ., by = "num_archive") %>%
    dplyr::select(-num_archive)

  if (nrow(archives_zip) > abs(n_fichiers)) {

    if (n_fichiers > 0) {
      archives_zip <- dplyr::filter(archives_zip, dplyr::row_number() <= n_fichiers)
    } else if (n_fichiers < 0) {
      archives_zip <- archives_zip %>%
        dplyr::filter(dplyr::row_number() > n() + n_fichiers)
    }

  }

  message("Décompression zip de ", length(archives_zip$archive_zip), " fichier(s)...")

  decompression <- archives_zip %>%
    split(1:nrow(.)) %>%
    pbapply::pblapply(function(ligne) {

      divr::zip_extract_file(ligne$archive_zip,
                                  regex_fichier = regex_fichier)

    }, cl = clusters)

  if (paralleliser == TRUE) {
    divr::cl_stop(clusters)
  }

  if (return_tibble == TRUE) {
    archives_zip <- archives_zip %>%
      dplyr::mutate(repertoire_sortie = stringr::str_match(archive_zip, "(.+)/")[, 2],
                    fichier = paste0(repertoire_sortie, "/", fichier))
    return(archives_zip)
  }

}

#' Concatener des fichiers
#'
#' Concaténer des fichiers.
#'
#' @param fichiers_entree Vecteur de fichiers à concaténer.
#' @param fichier_sortie Chemin du fichier en sortie.
#'
#' @examples
#'
#' @export
concatener_fichiers <- function(fichiers_entree, fichier_sortie) {

  fichiers <- lapply(fichiers_entree, readLines)

  fichier_sortie <- file(fichier_sortie, "w")
  purrr::walk(fichiers, ~ writeLines(., fichier_sortie))
  close(fichier_sortie)
}
