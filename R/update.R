update_monthly <- function(release_date = NULL) {
  
  if(is.null(release_date)) {
    stop("you have to provide a `release_date`", call. = FALSE)
  }
  
  monthly_uk <- rsindex(td, gclass = "uk", freq = "monthly")
  monthly_countries <- rsindex(td, gclass = "countries", freq = "monthly")
  monthly_london <- rsindex(td, gclass = "london_effect", freq = "monthly")
  
  monthly_aggregate <- reduce_join(monthly_uk, monthly_countries, monthly_london)
  monthly_nuts1 <- rsindex(td)
  monthly_nuts2 <- rsindex(td, gclass = "nuts2") 
  monthly_nuts3 <- rsindex(td, gclass = "nuts3")
  
  write_data(monthly_aggregate, monthly_nuts1, monthly_nuts2, monthly_nuts3, release = release_date)
  
}
