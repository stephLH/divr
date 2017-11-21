#' remplacer_na
#'
#' @param courant \dots.
#' @param cible \dots.
#' @param remplacement \dots.
#'
#' @export
remplacer_na <- function(courant, cible, remplacement = "défaut"){

  if (length(courant) != length(cible)) {
    stop(paste0("Les vecteur courant et cible doivent être de même taille."), call. = FALSE)
  }

  if (class(courant) != class(cible)) {
    stop(paste0("Les vecteur courant et cible doivent être de même classe"), call. = FALSE)
  }

  if (remplacement == "défaut") {
    courant <- ifelse(!is.na(cible), cible, courant)
  } else if (remplacement == "na_courant") {
    courant[which(is.na(courant))] <- cible[which(is.na(courant))]
  }

  return(courant)
}
