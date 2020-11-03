#' @export
update_monthly <- function(td, release_name = NULL) {
  
  if(is.null(release_name)) {
    stop("you have to provide a `release_date`", call. = FALSE)
  }
  
  monthly_uk <- rsindex(td, gclass = "uk", freq = "monthly")
  monthly_countries <- rsindex(td, gclass = "countries", freq = "monthly")
  monthly_london <- rsindex(td, gclass = "london_effect", freq = "monthly")
  
  monthly_aggregate <- reduce_join(monthly_uk, monthly_countries, monthly_london)
  monthly_nuts1 <- rsindex(td)
  monthly_nuts2 <- rsindex(td, gclass = "nuts2") 
  monthly_nuts3 <- rsindex(td, gclass = "nuts3")
  
  write_data(monthly_aggregate, monthly_nuts1, monthly_nuts2, monthly_nuts3, release = release_name)
  
  list(aggregate = monthly_aggregate, nuts1 = monthly_nuts1, nuts2 = monthly_nuts2, nuts3 = monthly_nuts3)
  
}

#' @export
update_quarterly <- function(td, release_name = NULL) {
  
  if(is.null(release_name)) {
    stop("you have to provide a `release_date`", call. = FALSE)
  }
  quarterly_uk <- rsindex(td, gclass = "uk", freq = "quarterly")
  quarterly_countries <- rsindex(td, gclass = "countries", freq = "quarterly")
  quarterly_london <- rsindex(td, gclass = "london_effect", freq = "quarterly")
  
  quarterly_aggregate <- reduce_join(quarterly_uk, quarterly_countries, quarterly_london)
  quarterly_nuts1 <- rsindex(td, freq = "quarterly") # works
  quarterly_nuts2 <- rsindex(td, gclass = "nuts2", freq = "quarterly") # works
  quarterly_nuts3 <- rsindex(td, gclass = "nuts3", freq = "quarterly") # works
  
  write_data(quarterly_aggregate, quarterly_nuts1, quarterly_nuts2, quarterly_nuts3, release = release_name)
  
  list(aggregate = quarterly_aggregate, nuts1 = quarterly_nuts1, nuts2 = quarterly_nuts2, nuts3 = quarterly_nuts3)
  
}

#' @export
update_annual <- function(td, release_name = NULL) {
  
  if(is.null(release_name)) {
    stop("you have to provide a `release_date`", call. = FALSE)
  }
  
  annual_uk <- rsindex(td, gclass = "uk", freq = "annual")
  annual_countries <- rsindex(td, gclass = "countries", freq = "annual")
  annual_london <- rsindex(td, gclass = "london_effect", freq = "annual")
  
  annual_aggregate <- reduce_join(annual_uk, annual_countries, annual_london)
  annual_nuts1 <- rsindex(td, freq = "annual") # works
  annual_nuts2 <- rsindex(td, gclass = "nuts2", freq = "annual") # works
  annual_nuts3 <- rsindex(td, gclass = "nuts3", freq = "annual") # works
  
  write_data(annual_aggregate, annual_nuts1, annual_nuts2, annual_nuts3, release = release_name)
  
  list(aggregate = annual_aggregate, nuts1 = annual_nuts1, nuts2 = annual_nuts2, nuts3 = annual_nuts3)
  
}

#' @export
update <- function(td, release_name = NULL) {
  monthly <- update_monthly(td, release_name = release_name)
  quarterly <- update_quarterly(td, release_name = release_name)
  annual <- update_annual(td, release_name = release_name)
  
  list(monthly = monthly, quarterly = quarterly, annual = annual)
}
