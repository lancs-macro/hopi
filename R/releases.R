#'  Get the release dates for the hopi
#'  
#' @importFrom lubridate ymd %m-% ceiling_date days year quarter
#' @importFrom dplyr if_else mutate arrange
#' @export
release_dates <- function() {
  
  years <- 2020:2026
  start_months <- c(1,4,7,10)
  end_months <- c(3,6,9,12)
  
  start_ym <- expand.grid(year = years, month = start_months)
  end_ym <- expand.grid(year = years, month = end_months)
  
  start_dates <- ymd(paste(start_ym$year, start_ym$month,"1", sep = "-"))
  end_dates <- lubridate::ceiling_date(ymd(paste(end_ym$year, end_ym$month, "1", sep = "-")), "month") %m-% days(1)
  
  name_release <- paste0(year(start_dates), "-Q",quarter(start_dates))
  
  released_dirs <- list.dirs("data/", recursive = FALSE, full.names = FALSE)
  
  data.frame(
    from = start_dates,
    to = end_dates,
    name = name_release
  ) %>% 
    mutate(released = if_else(name %in% released_dirs, "X", ""))  %>% 
    arrange(from)
  
}


#' @rdname release_dates
#' @importFrom utils tail head
#' @export
avail_releases <- function() {
  release_dates() %>% 
    dplyr::filter(released == "X") %>% 
    dplyr::pull(name)
}


#' @rdname release_dates
#' @export
last_release <- function() {
  avail_releases() %>% 
    tail(1)
}


#' @rdname release_dates
#' @export 
next_release <- function() {
  release_dates() %>% 
    dplyr::filter(released != "X") %>% 
    head(1) %>% 
    dplyr::pull(name)
}

#' @rdname release_dates
#' @export
next_release_to_date <- function() {
  release_dates() %>% 
    dplyr::filter(released != "X") %>% 
    head(1) %>% 
    dplyr::pull(to)
}