

dir_create_carefully <- function(dirpath) {
  if(!fs::dir_exists(dirpath)) {
    fs::dir_create(dirpath)
    cat("Creating:", dirpath, "\n")
  }
}
csv_write_carefully <- function(x, csvpath) {
  if(!fs::file_exists(csvpath)) {
    readr::write_csv(x, csvpath)
    cat("Writing: ", csvpath, "\n")
  }
}

file_copy_carefully <- function(old, new) {
  if(!file.exists(new)) {
    fs::file_copy(old, new)
    cat("Writing: ", new, "\n")
  }
}

#' @importFrom dplyr starts_with
write_classification <- function() {
  if (file_exists("data/classification.csv")) {
    return(invisible(NULL))
  }
  outfile <- readRDS(system.file("all_nuts.rds", package = "hopi")) %>%
    select(starts_with("nuts"), starts_with("nm")) %>% 
    unique()
  csv_write_carefully(outfile, "data/classification.csv")
}

#' @importFrom rlang dots_list
write_data <- function(..., release) {
  basepath <- "data"
  dir_create_carefully(basepath)
  obj_vec <- rlang::dots_list(...)
  releasepath <- release
  chr_dots <- sapply(substitute(...()), deparse)
  chr_vec <- gsub("_", "/", chr_dots)
  freqpath <- fs::path_dir(chr_vec)
  if(!all(freqpath %in% c("monthly", "quarterly", "annual"))) {
    stop("non-standard path names")
  }
  csvpath <- paste0(fs::path_file(chr_vec), ".csv")
  if(!all(csvpath %in% c("aggregate.csv", "nuts1.csv", "nuts2.csv", "nuts3.csv"))) {
    stop("non-standard path names")
  }
  filepath <- paste(basepath, releasepath, freqpath, csvpath, sep="/")
  purrr::map(fs::path_dir(filepath), dir_create_carefully)
  pcsv <- purrr::map2(obj_vec, filepath, csv_write_carefully)
  # write_folder_desc(obj_vec, filepath)
  write_classification()
}

# json_write_carefully <- function(x, jsonpath) {
#   if(!fs::file_exists(jsonpath)) {
#     jsonlite::write_json(x, jsonpath, pretty = TRUE)
#     cat("Writing: ", jsonpath, "\n")
#   }
# }

# csv_to_json_carefully <- function(csvpath, jsonpath) {
#   if(!file_exists(jsonpath)) {
#     suppressMessages({
#       csv_obj <- readr::read_csv(csvpath)
#     })
#     jsonlite::write_json(csv_obj, jsonpath, pretty = TRUE)
#     cat("Writing: ", jsonpath, "\n")
#   }
# }

# tree_list <- function(path = ".") {
#   isdir <- file.info(path)$isdir
#   if (!isdir) {
#     out <- path
#   } else {
#     files <- list.files(path, full.names   = TRUE, include.dirs = TRUE)
#     out <- lapply(files, tree_list)
#     names(out) <- basename(files)
#   }
#   out
# }

# write_docs_index <- function() {
#   index_list <- tree_list("docs")
#   jsonlite::write_json(index_list, "docs/index.json", pretty = TRUE)
#   cat("Writing:  docs/index.json\n")
# }

# write_docs_latest_note <- function(x) {
#   dirs <- list.dirs("docs/", recursive = FALSE, full.names = FALSE)
#   latest <- sort(dirs, decreasing = TRUE)[1]
#   json_write_carefully(latest, "docs/latest.json")
# }



# copy_desc <- function() {
#   descfiles <- list.files("data", pattern = "json", recursive = TRUE,)
#   desc_data_fullpath <- paste("data", descfiles, sep = "/")
#   desc_docs_fullpath <- paste("docs", descfiles, sep = "/")
#   pjson <- purrr::map2(desc_data_fullpath, desc_docs_fullpath, file_copy_carefully)
#   
# }

# write_docs <- function() {
#   basepath <- "docs"
#   dir_create_carefully(basepath)
#   csvfiles <- list.files("data", pattern = "csv", recursive = TRUE,)
#   jsonfiles <- gsub(".csv", ".json", csvfiles)
#   json_fullpath <- paste(basepath, jsonfiles, sep = "/")
#   purrr::map(fs::path_dir(json_fullpath), dir_create_carefully)
#   csv_fullpath <- paste("data", csvfiles, sep = "/")
#   pjson <- map2(csv_fullpath, json_fullpath, csv_to_json_carefully)
#   copy_desc()
#   write_docs_index()
#   write_docs_latest_note()
# }

# write_folder_desc <- function(x, filepath) {
#   destfile <- paste0(fs::path_dir(filepath)[1], "/desc.json")
#   if(file_exists(destfile)) {
#     return(invisible(NULL))
#   }
#   discard_idx <- map(x, attributes) %>% 
#     map(~ !names(.x) %in% c("row.names", "class"))
#   nms <- gsub(".csv", "", fs::path_file(filepath))
#   desc <- map(x, attributes) %>% 
#     map2(discard_idx, ~ .x[.y]) %>% 
#     set_names(nms)
#   json_write_carefully(desc, destfile)
# }
