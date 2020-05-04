# Download and Clean ------------------------------------------------------

#' Downloads the raw lang-registry file from amazon servers
#' 
#' The reason I am not using wget is that it has to be installed externally
#' `downlad.file` may truncate the downloaded fileif it used for files > 2G.
#' Try to use it one per session / or restart the session and use `gc()`.
#' @importFrom utils download.file
#' @export
download_lr_file <- function() {
  download.file("http://prod.publicdata.landregistry.gov.uk.s3-website-eu-west-1.amazonaws.com/pp-complete.csv", 
                destfile = "data/raw.csv")
}

#' Sources the `data-preparation.sh` script, which cleans the lr-file for commas 
#' 
#' @export
tidy_lr_file <- function() {
  shell("inst/data-preparation.sh")
}