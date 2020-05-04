# After estimation --------------------------------------------------------

#' @importFrom zoo coredata index
#' @importFrom tibble tibble 
#' @importFrom ggplot2 theme geom_line ggplot aes ggtitle element_blank 
plot_price_uk <- function(x) {
  tibble(
    uk = coredata(x)[[1]],
    idx = index(x[[1]])
  ) %>% 
    ggplot(aes(idx, uk)) + 
    geom_line() +
    theme_bw() + 
    ggtitle("United Kingdom") +
    theme(
      axis.title = element_blank(),
      strip.background = element_blank()
    )
}

#' @importFrom tidyr gather
#' @importFrom ggplot2 ggplot geom_line facet_wrap theme_bw theme
plot_price <- function(x) {
  gg <- x %>% 
    gather(region, value, -index, factor_key = TRUE) %>% 
    ggplot(aes(index, value)) + 
    geom_line() +
    facet_wrap(~region, scales = "free_y") +
    theme_bw() + 
    theme(
      axis.title = element_blank(),
      strip.background = element_blank()
    )
  gg
}