#' Append multiples files into one.
#'
#' @param input_files A vector of files to append.
#' @param output_file An output file.
#'
#' @export
files_append <- function(input_files, output_file) {

  files <- lapply(input_files, readLines)

  output_file <- file(output_file, "w")
  purrr::walk(files, ~ writeLines(., output_file))
  close(output_file)
}
