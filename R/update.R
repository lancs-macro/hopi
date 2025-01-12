#' Update a version
#' 
#' @param td the output of `process_data()`.
#' @param release_name how to name the release, defaults to `next_release()`.
#' 
#' @export
update <- function(td, release_name = next_release(), save = TRUE) {
  monthly <- update_monthly(td, release_name = release_name, save = save)
  quarterly <- update_quarterly(td, release_name = release_name, save = save)
  annual <- update_annual(td, release_name = release_name, save = save)
  
  list(monthly = monthly, quarterly = quarterly, annual = annual)
}

#' @rdname update
#' @export
update_monthly <- function(td, release_name = next_release(), save = TRUE) {
  
  if(is.null(release_name)) {
    stop("you have to provide a `release_date`", call. = FALSE)
  }
  
  monthly_uk <- rsindex(td, gclass = "uk", freq = "monthly")
  print("Completed: monthly_uk")
  monthly_countries <- rsindex(td, gclass = "countries", freq = "monthly")
  print("Completed: monthly_countries")
  monthly_london <- rsindex(td, gclass = "london_effect", freq = "monthly")
  print("Completed: monthly_london")
  
  monthly_aggregate <- reduce_join(monthly_uk, monthly_countries, monthly_london)
  print("Completed: monthly_aggregate")
  monthly_nuts1 <- rsindex(td)
  print("Completed: monthly_nuts1")
  monthly_nuts2 <- rsindex(td, gclass = "nuts2") 
  print("Completed: monthly_nuts2")
  monthly_nuts3 <- rsindex(td, gclass = "nuts3")
  print("Completed: monthly_nuts3")
  
  if (save) {
    write_data(monthly_aggregate, monthly_nuts1, monthly_nuts2, monthly_nuts3, release = release_name)
  }
  
  list(aggregate = monthly_aggregate, nuts1 = monthly_nuts1, nuts2 = monthly_nuts2, nuts3 = monthly_nuts3)
}

#' @rdname update
#' @export
update_quarterly <- function(td, release_name = next_release(), save = TRUE) {
  
  if(is.null(release_name)) {
    stop("you have to provide a `release_date`", call. = FALSE)
  }
  
  quarterly_uk <- rsindex(td, gclass = "uk", freq = "quarterly")
  print("Completed: quarterly_uk")
  quarterly_countries <- rsindex(td, gclass = "countries", freq = "quarterly")
  print("Completed: quarterly_countries")
  quarterly_london <- rsindex(td, gclass = "london_effect", freq = "quarterly")
  print("Completed: quarterly_london")
  
  quarterly_aggregate <- reduce_join(quarterly_uk, quarterly_countries, quarterly_london)
  print("Completed: quarterly_aggregate")
  quarterly_nuts1 <- rsindex(td, freq = "quarterly")
  print("Completed: quarterly_nuts1")
  quarterly_nuts2 <- rsindex(td, gclass = "nuts2", freq = "quarterly")
  print("Completed: quarterly_nuts2")
  quarterly_nuts3 <- rsindex(td, gclass = "nuts3", freq = "quarterly")
  print("Completed: quarterly_nuts3")
  
  if (save) {
    write_data(quarterly_aggregate, quarterly_nuts1, quarterly_nuts2, quarterly_nuts3, release = release_name)  
  }
  
  list(aggregate = quarterly_aggregate, nuts1 = quarterly_nuts1, nuts2 = quarterly_nuts2, nuts3 = quarterly_nuts3)
}

#' @rdname update
#' @export
update_annual <- function(td, release_name = next_release(), save = TRUE) {
  
  if(is.null(release_name)) {
    stop("you have to provide a `release_date`", call. = FALSE)
  }
  
  annual_uk <- rsindex(td, gclass = "uk", freq = "annual")
  print("Completed: annual_uk")
  annual_countries <- rsindex(td, gclass = "countries", freq = "annual")
  print("Completed: annual_countries")
  annual_london <- rsindex(td, gclass = "london_effect", freq = "annual")
  print("Completed: annual_london")
  
  annual_aggregate <- reduce_join(annual_uk, annual_countries, annual_london)
  print("Completed: annual_aggregate")
  annual_nuts1 <- rsindex(td, freq = "annual")
  print("Completed: annual_nuts1")
  annual_nuts2 <- rsindex(td, gclass = "nuts2", freq = "annual")
  print("Completed: annual_nuts2")
  annual_nuts3 <- rsindex(td, gclass = "nuts3", freq = "annual")
  print("Completed: annual_nuts3")
  
  if (save) {
    write_data(annual_aggregate, annual_nuts1, annual_nuts2, annual_nuts3, release = release_name)
  }
  
  list(aggregate = annual_aggregate, nuts1 = annual_nuts1, nuts2 = annual_nuts2, nuts3 = annual_nuts3)
}
