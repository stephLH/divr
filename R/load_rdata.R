#' Load an objet from a RData file.
#'
#' @param rdata RData file path.
#' @param object Object name to load.
#'
#' @return If parameter \code{object} stays \code{NULL} then all objects of the RData file loaded in a named list.\cr
#' Otherwise, only the object specified is loaded.
#'
#' @examples
#' divr::load_rdata(paste0(racine_packages, "divr/data/load_rdata.RData"))
#' divr::load_rdata(paste0(racine_packages, "divr/data/load_rdata.RData"), "lib_num_mois")
#'
#' @export
load_rdata <- function(rdata, object = NULL){

  if (!file.exists(rdata)) {
    stop(paste0("File \"", rdata, "\" doas not exist"), call. = FALSE)
  }

  env = new.env()
  load(file = rdata, envir = env)

  rdata_file <- stringr::str_match(rdata, "([^\\/]+?\\.RData)$")[, 2]

  if (length(env) == 1 & paste0(names(env)[1], ".RData") == rdata_file) {
    load_rdata <- env[[names(env)]]
    return(load_rdata)
  }

  if (!is.null(object)) {
    load_rdata <- env[[object]]
  } else {
    load_rdata <- as.list(env)
  }

  return(load_rdata)
}
