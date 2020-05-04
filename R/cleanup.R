cleanup <- function() {
  if(file_exists("temp")) {
    fs::dir_delete("temp")
  }
}