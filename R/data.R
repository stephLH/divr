#' Extract duplicate rows from a data frame.
#'
#' @param A data frame.
#'
#' @return A data frame containing only the duplicate rows.
#'
#' @examples
#' data <- dplyr::tibble(cle1 = c("A", "A", "B", "B"), cle2 = c("1", "1", "2", "3"), champ = 1:4)
#'
#' # With duplicate
#' divr::duplicate(data, cle1, cle2)
#'
#' # Without duplicate
#' divr::duplicate(data, cle1, cle2, champ))
#'
#' @export
duplicate <- function(data, ...){

  if (any(class(data) == "data.frame") == FALSE) {
    stop("The first paramater must be a data frame", call. = FALSE)
  }

  group_by <- dplyr::quos(...)

  duplicate <- dplyr::group_by(data, !!!group_by) %>%
    dplyr::filter(dplyr::row_number() >= 2) %>%
    dplyr::ungroup() %>%
    dplyr::select(purrr::map_chr(group_by, dplyr::quo_name)) %>%
    unique() %>%
    dplyr::right_join(data, ., by = purrr::map_chr(group_by, dplyr::quo_name))

  return(duplicate)
}

#' Patch a current vector from a target vector.
#'
#' @param current A vector to be patched.
#' @param target A patch vector. If length 1 then recycled to the length of current and only NA are patched.
#' @param only_na If \code{TRUE} then only missing values in current are patched.
#'
#' @return A patched vector.
#'
#' @examples
#' divr::patch_vector(c(1, NA_real_, 3), c(4, 5, 6))
#' divr::patch_vector(c(1, NA_real_, 3), c(4, 5, 6), only_na = TRUE)
#' divr::patch_vector(c(1, NA_real_, 3), 4)
#'
#' @export
patch_vector <- function(current, target, only_na = FALSE){

  if (length(target) == 1) {
    target <- rep(target, length(current))
    only_na <- TRUE
  }

  if (length(current) != length(target)) {
    stop(paste0("current and target vector must have the same length"), call. = FALSE)
  } else if (class(current) != class(target)) {
    stop(paste0("current and target vector must have the same class"), call. = FALSE)
  }

  if (only_na == FALSE) {
    current <- ifelse(!is.na(target), target, current)
  } else if (only_na == TRUE) {
    current[which(is.na(current))] <- target[which(is.na(current))]
  }

  return(current)
}

#' remplacer_si
#'
#' @param courant \dots
#' @param cible \dots
#' @param condition \dots
#' @param na_cible \dots
#'
#' @export
remplacer_si <- function(courant, cible, condition = TRUE, na_cible = FALSE){

  if (length(cible) == 1) {
    cible <- rep(cible, length(courant))
  }

  if (length(courant) != length(cible)) {
    stop(paste0("Les vecteur courant et cible doivent être de même taille."), call. = FALSE)
  } else if (class(courant) != class(cible)) {
    stop(paste0("Les vecteur courant et cible doivent être de même classe"), call. = FALSE)
  }

  if (length(condition) == 1 & length(courant) >= 2) {
    if (condition == TRUE) {
      condition <- rep(TRUE, length(courant))
    }
  }

  if (na_cible == FALSE) {
    condition <- condition & !is.na(cible)
  }

  courant <- ifelse(condition, cible, courant)


  return(courant)
}

#' maj_champ
#'
#' @param table \dots
#' @param table_pivot \dots
#' @param champ_maj \dots
#' @param doublons \dots
#'
#' @export
maj_champ <- function(table, table_pivot, champ_maj, ..., doublons = TRUE) {

  cle_noms <- dplyr::quos(...) %>%
    purrr::map_chr(dplyr::quo_name)

  champ_maj <- dplyr::enquo(champ_maj)
  nom_champ_maj <- dplyr::quo_name(champ_maj)

  table_pivot <- table_pivot %>%
    dplyr::select(cle_noms, .champ_maj = !!champ_maj)

  if (doublons == FALSE) {
    table_pivot <- table_pivot %>%
      dplyr::anti_join(divr::duplicate(table_pivot, ...), by = cle_noms)
  }

  maj_champ <- table %>%
    dplyr::filter(is.na(!!champ_maj)) %>%
    dplyr::left_join(table_pivot, by = cle_noms) %>%
    dplyr::mutate(!!nom_champ_maj := divr::patch_vector(!!champ_maj, .champ_maj, remplacement = "na_courant")) %>%
    dplyr::select(-.champ_maj)

  ajout <- tidyr::drop_na(table, !!champ_maj)

  maj_champ <- dplyr::bind_rows(maj_champ, ajout)

  return(maj_champ)
}

#' anti_join_bind
#'
#' @param x Table pivot
#' @param y Table tierce
#' @param by \dots
#' @param arrange \dots
#'
#' @export
anti_join_bind <- function(x, y, by, arrange = TRUE) {

  anti_join_bind <- y %>%
    dplyr::anti_join(x, by) %>%
    dplyr::bind_rows(x, .)

  if (arrange == TRUE) {
    anti_join_bind <- dplyr::arrange(anti_join_bind, !!!rlang::parse_quosure(by))
  }

  return(anti_join_bind)
}
