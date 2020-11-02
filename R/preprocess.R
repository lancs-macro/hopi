# lr_file <- function() {
#   "http://prod.publicdata.landregistry.gov.uk.s3-website-eu-west-1.amazonaws.com/pp-complete.csv"
# }

lr_url <- "http://prod.publicdata.landregistry.gov.uk.s3-website-eu-west-1.amazonaws.com/pp-complete.csv"


# Download and Clean ------------------------------------------------------

#' Downloads the raw land-registry file from amazon servers
#' 
#' The reason I am not using wget is that it has to be installed externally
#' `downlad.file` may truncate the downloaded file if it used for files > 2G.
#' Try to use it one per session / or restart the session and use `gc()`.
#' @importFrom utils download.file
#' @export
download_lr_file <- function() {
  if(!dir_exists("temp")) {
    fs::dir_create("temp")
  }
  if(file_exists("temp/raw.csv")) {
    stop("file already exists")
  }
  download.file(lr_file(), destfile = "temp/raw.csv")
}

#' Sources the `data-preparation.sh` script, which cleans the raw-file for commas 
#' 
#' @export
tidy_lr_file <- function() {
  if (!file_exists("temp/raw.csv")) {
    stop("There is no 'temp/raw.csv' file.")
  }
  if(file_exists("temp/main.csv")) {
    stop("file already exists")
  }
  shell("data-preparation.sh")
}


lr_url <- "http://prod.publicdata.landregistry.gov.uk.s3-website-eu-west-1.amazonaws.com/pp-complete.csv"


cleanup <- function() {
  if(file_exists("temp")) {
    fs::dir_delete("temp")
  }
}

#' Write a csv  
#' 
#' @importFrom fs dir_create dir_exists
#' @importFrom dplyr mutate select full_join case_when
#' @importFrom tidyr drop_na
#' @importFrom magrittr set_names
#' @importFrom readr read_csv write_csv
#' @importFrom uklr ons_lookup
write_nuts <- function() {
  
  if(!dir_exists("temp")) {
    fs::dir_create("temp")
  }
  # Download ----------------------------------------------------------------
  # URL_nuts1 <- "https://opendata.arcgis.com/datasets/01fd6b2d7600446d8af768005992f76a_0.csv"
  # URL_nuts2 <- "https://opendata.arcgis.com/datasets/f021bab88bab4d14b72fa8f17363f4a3_0.csv"
  # URL_nuts3 <- "https://opendata.arcgis.com/datasets/6da7ad08e66e4e68a616b95f50d92d5a_0.csv"
  # POSTAL CODE TO NUTS3
  # download.file("http://ec.europa.eu/eurostat/tercet/download.do?file=pc2018_uk_NUTS-2016_v2.0.zip",
  #               destfile = "temp/zip_pc.zip")
  # unzip("temp/zip_pc.zip")
  
  # Read --------------------------------------------------------------------
  
  
  nuts123 <- ons_lookup() %>% 
    select(starts_with("NUTS")) %>% 
    set_names(c("cd3", "nm3", "cd2", "nm2", "cd", "nm")) %>% 
    unique()
  
  pc_nuts3 <- uklr::pc %>% 
    set_names(c("cd3", "pc")) %>% 
    mutate(pc_trim =  gsub("[0-9].*","", pc)) 
  
  all_nuts_reduced <- full_join(nuts123, pc_nuts3, by = "cd3") %>% 
    select(Postcode = pc, pc_trim, nuts1 = cd,  nuts2 = cd2, nuts3 = cd3, nm, nm2, nm3) 
    
  # nuts1 <- read_csv(URL_nuts1) %>% 
  #   mutate(cd = nuts118cd,
  #          nm = nuts118nm) %>% 
  #   select(cd, nm)
  # 
  # nuts2 <- read_csv(URL_nuts2) %>% 
  #   mutate(cd2 = NUTS215CD,
  #          nm2 = NUTS215NM) %>% 
  #   select(cd2, nm2) %>% 
  #   mutate(cd = str_sub(cd2, 1, -2)) %>% 
  #   select(cd, cd2, nm2)
  # 
  # nuts3 <- read_csv(URL_nuts3) %>% 
  #   mutate(cd3 = NUTS315CD,
  #          nm3 = NUTS315NM) %>% 
  #   select(cd3, nm3) %>% 
  #   mutate(cd = str_sub(cd3, 1, -3),
  #          cd2 = str_sub(cd3, 1, -2)) %>% 
  #   select(cd, cd2, cd3, nm3)
  # 
  # # LENGTH 1,759,911 .. exlcuding NORTHERN IRELAND 
  # pc1_nuts3 <- read_delim("temp/pc2018_uk_NUTS-2016_v2.0.csv", 
  #                         ";", quote = "'", escape_double = FALSE, 
  #                         trim_ws = TRUE) %>% 
  #   set_names("cd3", "pc") %>% 
  #   mutate(pc_trim =  gsub("[0-9].*","", pc))
  # 
  # all_nuts <- full_join(nuts1, nuts2, by = "cd") %>% 
  #   full_join(nuts3, by = c("cd", "cd2")) %>% 
  #   full_join(pc1_nuts3, by = "cd3") %>% 
  #   select(cd, cd2, cd3, pc, pc_trim, nm, nm2, nm3) %>% 
  #   drop_na() %>% # HAD TO DROP NA TO SEE IF THIS IS CORRECT
  #   select(Postcode = pc, pc_trim, nuts1 = cd,  nuts2 = cd2, nuts3 = cd3, nm, nm2, nm3) 
  # 
  # uk <- c("UKI","UKJ","UKF","UKG","UKH","UKC","UKK", "UKD", "UKM", "UKN", "UKL", "UKE")
  england <- c("UKI","UKJ","UKF","UKG","UKH","UKC","UKK", "UKD", "UKE")
  london <- "UKI"
  northern_ireland <- "UKN"
  scotland <- "UKM"
  wales <- "UKL"
  uk <- c(england, scotland, northern_ireland, wales)
  uk_without_london <- uk[!uk == london]
  
  all_nuts <- all_nuts_reduced %>% 
    mutate(
      countries = case_when(
        nuts1 %in% england ~ "England",
        nuts1 %in% northern_ireland ~ "Northern Ireland",
        nuts1 %in% scotland ~ "Scotland",
        nuts1 %in% wales ~ "Wales"),
      uk = "United Kingdom",
      london_effect = ifelse(nuts1 %in% uk_without_london, "London Effect", NA_character_)
    )
  saveRDS(all_nuts, "inst/all_nuts.rds", compress = "xz")
  # readr::write_csv(all_nuts, "temp/nuts123pc.csv")
}
