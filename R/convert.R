#' Convert a local HTML file to PDF using Google Chrome
#'
#' @param  html_file \dots
#' @param  pdf_file \dots
#' @param  remove_html \dots
#'
#' @export
chrome_print_local <- function(html_file, pdf_file = NULL, remove_html = FALSE) {

  chrome_path <- pagedown::find_chrome()

  html_file <- tools::file_path_as_absolute(html_file)

  if (Sys.info()[["sysname"]] == "Windows") {
    prefix_html_file <- "file:///"
  } else {
    prefix_html_file <- NA_character_
  }

  if (is.null(pdf_file)) {
    pdf_file <- stringr::str_replace(html_file, "\\.html$", ".pdf")
  }

  system(glue::glue("\"{chrome_path}\" --headless --disable-gpu --print-to-pdf=\"{pdf_file}\" \"{prefix_html_file}{html_file}\""), intern = TRUE)

  if (remove_html == TRUE) {
    suppression <- file.remove(html_file)
  }

  return(pdf_file)

}
