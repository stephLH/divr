#' Extraire le chemin a partir du chemin complet
#'
#' Extraire le chemin à partir du chemin complet.
#'
#' @param chemin_fichier Chemin complet dont le chemin est extrait.
#'
#' @return Le chemin.
#'
#' @examples
#' divr::extraire_chemin(c("Test/Dossier/fichier_excel.xlsx", "Test2/Dossier2/fichier_word.docx"))
#'
#' @export
extraire_chemin <- function(chemin_fichier) {

  chemin <- stringr::str_match(chemin_fichier, "(.+)/[^/]+?$")[, 2]

  return(chemin)
}

#' Extraire des fichiers d'une archive zip
#'
#' Extraire des fichiers d'une archive zip.
#'
#' @param archive_zip Chemin du fichier zip dont le contenu est extrait.
#' @param regex_fichier Expression régulière pour filtrer les fichiers à extraire.
#' @param repertoire_sortie Chemin du répertoire vers lequel est réalisée l'extraction (par défaut, le répertoire de l'archive zip).
#' @param supprimer_zip \code{TRUE}: l'archive zip est supprimée; \code{FALSE}: l'archive zip n'est pas supprimée..
#'
#' @examples
#' # Extraire les fichiers pdf d'une archive
#' divr::extraire_fichiers_zip("archive.zip", regex_fichier = "pdf$", repertoire_sortie = "test_zip")
#'
#' @export
extraire_fichiers_zip <- function(archive_zip, regex_fichier = NULL, repertoire_sortie = NULL, supprimer_zip = FALSE) {

  if (!file.exists(archive_zip)) {
    stop("L'archive \"", archive_zip,"\" n'existe pas.", call. = FALSE)
  }

  fichiers <- unzip(archive_zip, list = TRUE)[["Name"]]

  if (!is.null(regex_fichier)) fichiers <- fichiers[stringr::str_detect(fichiers, regex_fichier)]

  if (is.null(repertoire_sortie)) {
    repertoire_sortie <- stringr::str_match(archive_zip, "(.+)/")[, 2]
  }

  unzip(archive_zip, fichiers, exdir = repertoire_sortie)

  if (supprimer_zip == TRUE) {
    file.remove(archive_zip) %>%
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

      divr::extraire_fichiers_zip(ligne$archive_zip,
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
