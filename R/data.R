#' data
#'
#' @export
#' @keywords internal
data <- function() {

  lib_num_mois <- importr::importer_table_access("libelle_num_mois", paste0(racine_packages, "divr/Tables_ref.accdb"))
  save("lib_num_mois", file = paste0(racine_packages, "divr/data/lib_num_mois.RData"))

}
