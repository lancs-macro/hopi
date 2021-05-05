# https://geoportal.statistics.gov.uk/datasets/rural-urban-classification-2011-of-nuts-3-2015-in-england/data?page=9


write_nuts_ruc <- function() {
  ruc <- read_csv("inst/Rural_Urban_Classification_(2011)_of_NUTS_3_(2015)_in_England.csv") %>% 
    select(nuts3 = NUTS315CD, nm3 = NUTS315NM, ruc = RUC11, bruc = Broad_RUC11) 
  nuts <- read_rds("inst/nuts.rds")
  nuts_ruc <- left_join(nuts, ruc, by = c("nuts3", "nm3")) %>% 
    mutate(bruc = recode(
      bruc, 
      `Predominantly Urban` = "urban", 
      `Urban with Significant Rural` = "urban2",
      `Predominantly Rural` = "rural"))
  
  saveRDS(nuts_ruc, "inst/nuts_ruc.rds")
  
}
