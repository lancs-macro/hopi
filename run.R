

# Preprocess --------------------------------------------------------------

# download_lr_file()
# tidy_lr_file()

# Clean data --------------------------------------------------------------

# td <- tidy_data()

# monthly -----------------------------------------------------------------

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


monthly_uk <- rsindex(td, gclass = "uk", freq = "monthly")
monthly_countries <- rsindex(td, gclass = "countries", freq = "monthly")
monthly_london <- rsindex(td, gclass = "london_effect", freq = "monthly")

monthly_aggregate <- reduce_join(monthly_uk, monthly_countries, monthly_london)
monthly_nuts1 <- rsindex(td)
monthly_nuts2 <- rsindex(td, gclass = "nuts2") 
monthly_nuts3 <- rsindex(td, gclass = "nuts3")

write_data(monthly_aggregate, monthly_nuts1, monthly_nuts2, monthly_nuts3, release = "2020-04")

# quarterly ---------------------------------------------------------------

quarterly_uk <- rsindex(td, gclass = "uk", freq = "quarterly")
quarterly_countries <- rsindex(td, gclass = "countries", freq = "quarterly")
quarterly_london <- rsindex(td, gclass = "london_effect", freq = "quarterly")

quarterly_aggregate <- reduce_join(quarterly_uk, quarterly_countries, quarterly_london)
quarterly_nuts1 <- rsindex(td, freq = "quarterly") # works
quarterly_nuts2 <- rsindex(td, gclass = "nuts2", freq = "quarterly") # works
quarterly_nuts3 <- rsindex(td, gclass = "nuts3", freq = "quarterly") # works

write_data(quarterly_aggregate, quarterly_nuts1, quarterly_nuts2, quarterly_nuts3, release = "2020-04")

# annual ------------------------------------------------------------------

annual_uk <- rsindex(td, gclass = "uk", freq = "annual")
annual_countries <- rsindex(td, gclass = "countries", freq = "annual")
annual_london <- rsindex(td, gclass = "london_effect", freq = "annual")

annual_aggregate <- reduce_join(annual_uk, annual_countries, annual_london)
annual_nuts1 <- rsindex(td, freq = "annual") # works
annual_nuts2 <- rsindex(td, gclass = "nuts2", freq = "annual") # works
annual_nuts3 <- rsindex(td, gclass = "nuts3", freq = "annual") # works

write_data(annual_aggregate, annual_nuts1, annual_nuts2, annual_nuts3, release = "2020-04")

# write_docs()

# nuts1_weekly <- rsindex(td, freq = "weekly")
# nuts1_daily <- rsindex(td, freq = "daily")

# cleanup()



  
