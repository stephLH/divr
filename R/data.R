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
