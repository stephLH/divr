#' data
#'
#' @export
data <- function() {

  data_lib_num_mois <- importr::importer_table_access("libelle_num_mois", paste0(racine_packages, "divr/data/Tables_ref.accdb"))
  save("data_lib_num_mois", file = paste0(racine_packages, "divr/data/data_lib_num_mois.RData"))

}
