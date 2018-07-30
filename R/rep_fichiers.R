#' Creer un repertoire
#'
#' Créer un répertoire.
#'
#' @param repertoire Chemin du répertoire à créer.
#'
#' @examples
#' # Création du repertoire "test"
#' divr::creer_repertoire("test")
#'
#' @export
creer_repertoire <- function(repertoire) {

  if (class(repertoire) != "character") {
    stop("Le paramètre doit être de type character (chemin vers le répertoire)", call. = FALSE)
  }

  if (!dir.exists(repertoire)) {
    dir.create(repertoire, recursive = TRUE)
  }
}

#' Vider un repertoire
#'
#' Vider un repertoire.
#'
#' @param repertoire Chemin du répertoire à vider.
#'
#' @examples
#' # Création du repertoire "test"
#' divr::creer_repertoire("test")
#'
#' # Ecriture d'un fichier à l'intérieur
#' write.csv(1, "test/test.csv")
#'
#' # Vidage du répertoire
#' divr::vider_repertoire("test")
#'
#' @export
vider_repertoire <- function(repertoire) {

  if (class(repertoire) != "character") {
    stop("Le paramètre doit être de type character (chemin vers le répertoire)", call. = FALSE)
  }

  if (dir.exists(repertoire)) {
    supprimer_repertoire(repertoire)
  } else message("Le répertoire \"", repertoire, "\" n'existait pas.")

  Sys.sleep(0.25)
  dir.create(repertoire)
}

#' Extraire l'extension d'un fichier
#'
#' Extraire l'extension d'un fichier.
#'
#' @param fichier Chemin du fichier dont l'extension est extraite.
#'
#' @return L'extension du fichier.
#'
#' @examples
#' divr::extension_fichier(c("fichier_excel.xlsx", "fichier_word.docx"))
#'
#' @export
extension_fichier <- function(fichier) {

  if (class(fichier) != "character") {
    stop("Le paramètre doit être de type character", call. = FALSE)
  }

  extension_fichier <- stringr::str_match(fichier, "\\.([^\\.]+)$")[, 2]

  return(extension_fichier)
}

#' Extraire le fichier a partir du chemin complet
#'
#' Extraire le fichier à partir du chemin complet.
#'
#' @param chemin_fichier Chemin complet dont le fichier est extrait.
#'
#' @return Le nom du fichier.
#'
#' @examples
#' divr::extraire_nom_fichier(c("Dossier/fichier_excel.xlsx", "Dossier2/fichier_word.docx"))
#'
#' @export
  extraire_nom_fichier <- function(chemin_fichier) {

  nom_fichier <- stringr::str_match(chemin_fichier, "([^\\/]+?)$")[, 2]

  return(nom_fichier)
}

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

#' Supprimer un repertoire
#'
#' Supprimer un repertoire.
#'
#' @param repertoire Chemin du répertoire à supprimer.
#'
#' @examples
#' # Création du répertoire "test"
#' divr::creer_repertoire("test")
#'
#' # Suppression du répertoire
#' divr::supprimer_repertoire("test")
#'
#' @export
supprimer_repertoire <- function(repertoire) {

  if (class(repertoire) != "character") {
    stop("Le paramètre doit être de type character (le nom du répertoire)", call. = FALSE)
  }

  repertoire <- stringr::str_match(repertoire, "(.+?)(/)?$")[, 2]

  Sys.sleep(0.25)
  essai_suppression <- unlink(repertoire, recursive = TRUE)

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
    clusters <- divr::initialise_cluster()
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
    divr::stopper_cluster(clusters)
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
