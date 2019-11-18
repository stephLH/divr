#' Convert an HTML file to PDF using Google Chrome
#'
#' @param  html_file \dots
#' @param  pdf_file \dots
#' @param  remove_html \dots
#'
#' @export
convert_html_to_pdf <- function(html_file, pdf_file = NULL, remove_html = FALSE) {

  if (is.null(pdf_file)) {
    pdf_file <- stringr::str_replace(html_file, "\\.html$", ".pdf")
  }

  if (Sys.info()[["sysname"]] == "Windows") {

    pagedown::chrome_print(html_file, pdf_file)

  } else if (Sys.info()[["sysname"]] == "Linux") {

    chrome_path <- pagedown::find_chrome()

    system(glue::glue("\"{chrome_path}\" --headless --disable-gpu --print-to-pdf=\"{pdf_file}\" \"{html_file}\""), intern = TRUE)
  }

  if (remove_html == TRUE) {
    suppression <- file.remove(html_file)
  }

  return(pdf_file)

}
