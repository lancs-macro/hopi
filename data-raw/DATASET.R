## code to prepare `DATASET` dataset goes here



# Download ----------------------------------------------------------------

# NUTS 1
URL_nuts1 <- "https://opendata.arcgis.com/datasets/01fd6b2d7600446d8af768005992f76a_0.csv"
# temp_nuts1 <- "regional_info/nuts1.csv"
# download.file(URL_nuts1, destfile = temp_nuts1, mode = 'wb')

# NUTS 2
URL_nuts2 <- "https://opendata.arcgis.com/datasets/f021bab88bab4d14b72fa8f17363f4a3_0.csv"
# temp_nuts2 <- "regional_info/nuts2.csv"
# download.file(URL_nuts2, destfile = temp_nuts2, mode = 'wb')

# NUTS 3
URL_nuts3 <- "https://opendata.arcgis.com/datasets/6da7ad08e66e4e68a616b95f50d92d5a_0.csv"
# temp_nuts3 <- "regional_info/nuts3.csv"
# download.file(URL_nuts3, destfile = temp_nuts3, mode = 'wb')

# POSTAL CODE TO NUTS3
download.file("http://ec.europa.eu/eurostat/tercet/download.do?file=pc2018_uk_NUTS-2016_v2.0.zip",
              "data-raw/zip_pc.zip")
unzip("data-raw/zip_pc.zip", exdir = "regional_info")

# Read --------------------------------------------------------------------

library(tidyverse)

nuts1 <- read_csv(URL_nuts1) %>% 
  mutate(cd = nuts118cd,
         nm = nuts118nm) %>% 
  select(cd, nm)

nuts2 <- read_csv(URL_nuts2) %>% 
  mutate(cd2 = NUTS215CD,
         nm2 = NUTS215NM) %>% 
  select(cd2, nm2) %>% 
  mutate(cd = str_sub(cd2, 1, -2)) %>% 
  select(cd, cd2, nm2)

nuts3 <- read_csv(URL_nuts3) %>% 
  mutate(cd3 = NUTS315CD,
         nm3 = NUTS315NM) %>% 
  select(cd3, nm3) %>% 
  mutate(cd = str_sub(cd3, 1, -3),
         cd2 = str_sub(cd3, 1, -2)) %>% 
  select(cd, cd2, cd3, nm3)

# LENGTH 1,759,911 .. exlcuding NORTHERN IRELAND 
pc1_nuts3 <- read_delim("regional_info/pc2018_uk_NUTS-2016_v2.0.csv", 
                        ";", quote = "'", escape_double = FALSE, 
                        trim_ws = TRUE) %>% 
  set_names("cd3", "pc") %>% 
  mutate(pc_trim =  gsub("[0-9].*","", pc))

all_nuts <- full_join(nuts1, nuts2, by = "cd") %>% 
  full_join(nuts3, by = c("cd", "cd2")) %>% 
  full_join(pc1_nuts3, by = "cd3") %>% 
  select(cd, cd2, cd3, pc, pc_trim, nm, nm2, nm3) %>% 
  drop_na() %>% # HAD TO DROP NA TO SEE IF THIS IS CORRECT
  select(Postcode = pc, pc_trim, nuts1 = cd,  nuts2 = cd2, nuts3 = cd3, nm, nm2, nm3) 

uk <- c("UKI","UKJ","UKF","UKG","UKH","UKC","UKK", "UKD", "UKM", "UKN", "UKL", "UKE")
england <- c("UKI","UKJ","UKF","UKG","UKH","UKC","UKK", "UKD", "UKE")
northern_ireland <- "UKN"
scotland <- "UKM"
wales <- "UKL"

all_nuts <- all_nuts %>% 
  mutate(
    countries = case_when(
      nuts1 %in% england ~ "England",
      nuts1 %in% northern_ireland ~ "Northern Ireland",
      nuts1 %in% scotland ~ "Scotland",
      nuts1 %in% wales ~ "Wales"),
    uk = "United Kingdom"
  )

readr::write_csv(all_nuts, "data-raw/nuts123pc.csv")

# usethis::use_data(all_nuts, overwrite = TRUE)
