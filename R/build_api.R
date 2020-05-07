

#' @importFrom whisker whisker.render
render_template <- function(path, data) {
  template <- read_file(path)
  if (length(template) == 0)
    return("")
  whisker::whisker.render(template, data)
}

# Reading -----------------------------------------------------------------

read_file <- function(path) {
  lines <- read_lines(path)
  paste0(lines, "\n", collapse = "")
}

# Writing -----------------------------------------------------------------

#' @importFrom yaml as.yaml
write_yaml <- function(x, path) {
  write_lines(yaml::as.yaml(x), path = path)
}

# Inspired by roxygen2 utils-io.R (https://github.com/klutometis/roxygen/) -----------

readLines <- function(...) stop("Use read_lines!")
writeLines <- function(...) stop("Use write_lines!")

read_lines <- function(path, n = -1L) {
  base::readLines(path, n = n, encoding = "UTF-8", warn = FALSE)
}

write_lines <- function(text, path) {
  base::writeLines(enc2utf8(text), path, useBytes = TRUE)
}

# Other -------------------------------------------------------------------

#' @importFrom digest digest
#' @importFrom fs file_exists
file_equal <- function(src, dst) {
  if (!file_exists(dst))
    return(FALSE)
  
  src_hash <- digest::digest(file = src, algo = "xxhash64")
  dst_hash <-  digest::digest(file = dst, algo = "xxhash64")
  
  identical(src_hash, dst_hash)
}
