pkgs <- c("fs", "tidyverse", "data.table", "zoo", "bizdays",
          "RQuantLib", "ISOweek", "Matrix", "lubridate", "jsonlite")

#' Makes sures all the necessary packages are installed
setup_install <- function() {
  miss_pkgs <- pkgs[!(pkgs %in% installed.packages()[,"Package"])]
  if (length(miss_pkgs)) {
    install.packages(miss_pkgs)
  }
}

#' Makes sure that all the necessary packages are loaded
setup_load <- function() {
  invisible(lapply(pkgs, require, character.only = TRUE, quietly = TRUE))
}


# Json --------------------------------------------------------------------

gh_post(nuts1_monthly, path = "monthly/nuts1.json")
gh_post(nuts2_monthly, path = "monthly/nuts2.json")
gh_post(nuts3_monthly, path = "monthly/nuts3.json")


# Plotting ----------------------------------------------------------------

plot_price_uk(uk_monthly)


plot_price(nuts1_monthly) %>% 
  ggsave(filename = "nuts1_monthly.png", plot = ., path = "send", width = 10.5, height = 7)
plot_price(nuts2_monthly) %>% 
  ggsave(filename = "nuts2_monthly.png", plot = ., path = "send", width = 10.5, height = 8.5)
plot_price(nuts3_monthly) %>% 
  ggsave(filename = "nuts3_monthly.png", plot = ., path = "send", width = 10.5, height = 10)


plot_price(nuts1_quarterly) %>% 
  ggsave(filename = "nuts1_quarterly.png", plot = ., path = "send", width = 10.5, height = 7)
plot_price(nuts2_quarterly) %>% 
  ggsave(filename = "nuts2_quarterly.png", plot = ., path = "send", width = 10.5, height = 8.5)
plot_price(nuts3_quarterly) %>% 
  ggsave(filename = "nuts3_quarterly.png", plot = ., path = "send", width = 10.5, height = 10)


plot_price(nuts1_annual) %>% 
  ggsave(filename = "nuts1_annual.png", plot = ., path = "send", width = 10.5, height = 7)
plot_price(nuts2_annual) %>% 
  ggsave(filename = "nuts2_annual.png", plot = ., path = "send", width = 10.5, height = 8.5)
plot_price(nuts3_annual) %>% 
  ggsave(filename = "nuts3_annual.png", plot = ., path = "send", width = 10.5, height = 10)


plot_price(nuts1_weekly) %>% 
  ggsave(filename = "nuts1_weekly.png", plot = ., path = "send", width = 10.5, height = 7)
plot_price(nuts1_daily) %>% 
  ggsave(filename = "nuts1_daily.png", plot = ., path = "send", width = 10.5, height = 7)

# write into excel --------------------------------------------------------

library(writexl)

sheets_monthly <- list("nuts1" = nuts1_monthly, "nut2" = nuts2_monthly, "nut3" = nuts3_monthly) 
write_xlsx(sheets_monthly, "send/monthly.xlsx")

sheets_quarterly <- list("nuts1" = nuts1_quarterly, "nut2" = nuts2_quarterly, "nut3" = nuts3_quarterly) 
write_xlsx(sheets_quarterly, "send/quarterly.xlsx")

sheets_annual <- list("nuts1" = nuts1_annual, "nut2" = nuts2_annual, "nut3" = nuts3_annual) 
write_xlsx(sheets_annual, "send/annual.xlsx")


sheets_abs0.2 <- list(absret_0.2 = nuts1_monthly_absret0.2)
write_xlsx(sheets_abs0.2, "send/monthly_abs0.2.xlsx")


# In Development ----------------------------------------------------------


sheets_weekly <- list("nuts1" = nuts1_weekly)
write_xlsx(sheets_weekly, "send/weekly.xlsx")

sheets_daily <- list("nuts1" = nuts1_daily)
write_xlsx(sheets_daily, "send/daily.xlsx")
