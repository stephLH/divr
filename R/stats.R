#' Count aggregates (extended version of dplyr::count).
#'
#' @param data A data frame.
#' @param ... Variables to group by.
#' @param total_value The total value label.
#'
#' @return A tbl, grouped the same way as data.
#'
#' @examples
#' data <- dplyr::tibble(
#'   var1 = as.character(c(1, 1, 2, 2)),
#'   var2 = as.character(c(1, 2, 1, 2)),
#'   var3 = as.character(c(1, 2, 2, 2))
#' )
#' divr::count_agg(data, var1, var2)
#'
#' @export
count_agg <- function(data, ..., total_value = "Total") {

  fields <- dplyr::quos(...)
  fields_count_name <- purrr::map_chr(fields, rlang::as_name)
  fields_not_count_name <- names(data)[!names(data) %in% fields_count_name]

  count_agregat <- function(data, champ, total_value) {

    fields_count <- fields_count_name[-c(which(fields_count_name == champ):length(fields_count_name))]

    fields_mutate <- fields_count_name[c(which(fields_count_name == champ):length(fields_count_name))]
    mutate <- stats::setNames(as.list(rep(NA_character_, length(fields_mutate))),
                              as.list(fields_mutate))

    count_agregat <- data %>%
      dplyr::filter(!!rlang::parse_quo(glue::glue("!is.na({champ})"), env = rlang::caller_env())) %>%
      dplyr::count(!!!lapply(c(fields_not_count_name, fields_count), rlang::parse_quo, env = rlang::caller_env())) %>%
      dplyr::mutate(!!!mutate) %>%
      dplyr::mutate(!!champ := total_value)

    return(count_agregat)
  }

  count_agregats <- data %>%
    dplyr::count(!!!lapply(c(fields_not_count_name, fields_count_name), rlang::parse_quo, env = rlang::caller_env())) %>%
    dplyr::bind_rows(
      purrr::map_df(fields_count_name, ~ count_agregat(data, ., total_value = total_value))
    )

  return(count_agregats)
}

#' Round with a sum 100 (avoid round errors)
#'
#' @param x A numeric vector
#'
#' @return A round numeric vector
#'
#' @examples
#' x <- c(81.808219, 6.575342, 11.616438)
#'
#' sum(x)
#' sum(round(x))
#'
#' divr::round_100(x)
#' sum(divr::round_100(x))
#'
#' @export
round_100 <- function(x) {

  if (sum(round(x)) == 100) {
    return(round(x))
  }

  diff <- abs(x - round(x))
  position_max <- which(diff == max(diff))

  round_100 <- round(x)

  if (sum(round(x)) == 99) {
    round_100[position_max] <- round_100[position_max] + 1
  } else if (sum(round(x)) == 101) {
    round_100[position_max] <- round_100[position_max] - 1
  }

  return(round_100)
}

#' Return a base 100 serie
#'
#' @param x A numeric vector
#'
#' @return A base 100 vector
#'
#' @examples
#' x <- c(5096, 5077, 5278, 5352, 5437, 5387)
#'
#' divr::base_100(x)
#'
#' @export
base_100 <- function(x) {

  base_100 <- 100 + ((x - x[1]) / x[1] * 100)

  return(base_100)
}


