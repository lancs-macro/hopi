
plot_price(nuts1_weekly)

library(itsmr)

nr <- nrow(nuts1_weekly)

smooth_ma <- function(x, q = 2) {
  n = length(x)
  x = c(rep(x[1], q), x, rep(x[n], q))
  qq = -q:q
  fun = function(t) sum(x[t + qq])/(2 * q + 1)
  m = sapply((q + 1):(n + q), fun)
  m
}

nuts1_weekly_smooth_ma1 <- nuts1_weekly %>% 
  # slice(-c((nr-6):nr)) %>% 
  modify_at(vars(-index), ~ smooth_ma(.x, 1)) %>%
  gather(region, value, -index)


nuts1_weekly_smooth_ma4 <- nuts1_weekly %>% 
  # slice(-c((nr-6):nr)) %>% 
  modify_at(vars(-index), ~ smooth_ma(.x, 4)) %>%
  gather(region, value, -index)


library(zoo)

nuts1_quarterly <- readxl::read_excel("send/quarterly.xlsx") %>% 
  gather(region, value, -index) %>% 
  mutate(index = zoo::as.Date(as.yearqtr(index)))

nut1_weekly_quarterly_comparison <- ggplot() +
  geom_line(data = nuts1_quarterly, aes(index, value, col = "Quarterly")) +
  geom_line(data = nuts1_weekly_smooth_ma1, aes(index, value, col = "Weekly - ma(1)")) +
  # geom_line(data = nuts1_weekly_smooth_ma4, aes(index, value, col = "Weekly - ma(4)")) +
  facet_wrap(~region, scales = "free") +
  scale_colour_manual(name = "Calculation", values = c("red", "black", "green")) +
  theme_bw()

ggsave("nut1_weekly_quarterly_comparison.png", plot = nut1_weekly_quarterly_comparison, 
       path = "send", width = 10.5, height = 7)
