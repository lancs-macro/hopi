library(tidyverse)

df1 <- readxl::read_excel("send/monthly.xlsx") %>% 
  dplyr::select(-index) 
ts1 <- ts(df1, frequency = 12, start = c(1995, 2))


# readxl::read_excel("send/monthly.xlsx") %>% 
#   gather(region, price, -index) %>% 
#   ggplot(aes(index, price, col = region)) +
#   geom_line() +
#   theme_bw()


# Levels ------------------------------------------------------------------


acf_lvl <- df1 %>% 
  map_df(~ acf(.x, plot= FALSE)$acf)
pacf_lvl <- df1 %>% 
  map_df(~ pacf(.x, plot= FALSE)$acf)

ar_lvl <- df1 %>% 
  summarise_all(list(~ ar(.x)$ar))
ar_diff <- df1 %>% 
  mutate_all(~ log(.x) - dplyr::lag(log(.x))) %>% 
  drop_na() %>% 
  map(~ ar(.x)$ar) %>% 
  map_df(`[`, 1:10)


ts_sheets <- list("acf_lvl" = acf_lvl, "pacf_lvl" = pacf_lvl, "ar_lvl" = ar_lvl, "ar_diff" = ar_diff)
writexl::write_xlsx(ts_sheets, "send/ts_properties.xlsx")
