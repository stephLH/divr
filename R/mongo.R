#' Lancer le daemon Mongo a partir de la base
#'
#' Lancer le daemon Mongo à partir de la base
#'
#' @param chemin_base_mongo Le chemin vers la base de données Mongo.
#'
#' @examples
#' # Le dernier dossier du répertoire "_traitement/unzip" (Il s'agit du répertoire où sont dézippés les backups Mongo)
#' paste0(disque, "_traitement/unzip") %>%
#'   list.dirs(recursive = FALSE) %>%
#'   tail(1) %>%
#'   divr::mongo_daemon()
#'
#' @export
mongo_daemon <- function(chemin_base_mongo) {

  system2("C:/Program Files/MongoDB/Server/3.0/bin/mongod.exe", c("--dbpath", chemin_base_mongo), wait = FALSE)

}
