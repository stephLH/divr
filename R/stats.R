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
