# Timestamped progress line - cheap but worth it once a run spans hours/days
# and you're reading it back later to see where the time went.
log_msg <- function(...) cat(format(Sys.time(), "[%Y-%m-%d %H:%M:%S] "), ..., "\n")

# TRUE if every output file for this release+frequency is already on disk, so a
# crash-and-rerun doesn't recompute (and wait out) a stage that already finished.
release_files_exist <- function(release, frequency) {
  paths <- file.path("data", release, frequency, c("aggregate.csv", "nuts1.csv", "nuts2.csv", "nuts3.csv"))
  all(fs::file_exists(paths))
}

#' Update a version
#'
#' @param td the output of `process_data()`.
#' @param release_name how to name the release, defaults to `next_release()`.
#' @param verbose print per-region timing inside `rsindex()`.
#' @param workers passed to `rsindex()`; see its docs (parallelizes across regions,
#'   not helpful for single-region classes like "uk").
#'
#' @export
update <- function(td, release_name = next_release(), save = TRUE, verbose = FALSE, workers = 1) {
  monthly <- update_monthly(td, release_name = release_name, save = save, verbose = verbose, workers = workers)
  quarterly <- update_quarterly(td, release_name = release_name, save = save, verbose = verbose, workers = workers)
  annual <- update_annual(td, release_name = release_name, save = save, verbose = verbose, workers = workers)

  list(monthly = monthly, quarterly = quarterly, annual = annual)
}

#' @rdname update
#' @export
update_monthly <- function(td, release_name = next_release(), save = TRUE, verbose = FALSE, workers = 1) {

  if(is.null(release_name)) {
    stop("you have to provide a `release_date`", call. = FALSE)
  }
  if (save && release_files_exist(release_name, "monthly")) {
    log_msg("monthly", release_name, "already written, skipping")
    return(invisible(NULL))
  }

  monthly_uk <- rsindex(td, gclass = "uk", freq = "monthly", verbose = verbose, workers = workers)
  log_msg("Completed: monthly_uk")
  monthly_countries <- rsindex(td, gclass = "countries", freq = "monthly", verbose = verbose, workers = workers)
  log_msg("Completed: monthly_countries")
  monthly_london <- rsindex(td, gclass = "london_effect", freq = "monthly", verbose = verbose, workers = workers)
  log_msg("Completed: monthly_london")

  monthly_aggregate <- reduce_join(monthly_uk, monthly_countries, monthly_london)
  log_msg("Completed: monthly_aggregate")
  monthly_nuts1 <- rsindex(td, verbose = verbose, workers = workers)
  log_msg("Completed: monthly_nuts1")
  monthly_nuts2 <- rsindex(td, gclass = "nuts2", verbose = verbose, workers = workers)
  log_msg("Completed: monthly_nuts2")
  monthly_nuts3 <- rsindex(td, gclass = "nuts3", verbose = verbose, workers = workers)
  log_msg("Completed: monthly_nuts3")

  if (save) {
    write_data(monthly_aggregate, monthly_nuts1, monthly_nuts2, monthly_nuts3, release = release_name)
  }

  list(aggregate = monthly_aggregate, nuts1 = monthly_nuts1, nuts2 = monthly_nuts2, nuts3 = monthly_nuts3)
}

#' @rdname update
#' @export
update_quarterly <- function(td, release_name = next_release(), save = TRUE, verbose = FALSE, workers = 1) {

  if(is.null(release_name)) {
    stop("you have to provide a `release_date`", call. = FALSE)
  }
  if (save && release_files_exist(release_name, "quarterly")) {
    log_msg("quarterly", release_name, "already written, skipping")
    return(invisible(NULL))
  }

  quarterly_uk <- rsindex(td, gclass = "uk", freq = "quarterly", verbose = verbose, workers = workers)
  log_msg("Completed: quarterly_uk")
  quarterly_countries <- rsindex(td, gclass = "countries", freq = "quarterly", verbose = verbose, workers = workers)
  log_msg("Completed: quarterly_countries")
  quarterly_london <- rsindex(td, gclass = "london_effect", freq = "quarterly", verbose = verbose, workers = workers)
  log_msg("Completed: quarterly_london")

  quarterly_aggregate <- reduce_join(quarterly_uk, quarterly_countries, quarterly_london)
  log_msg("Completed: quarterly_aggregate")
  quarterly_nuts1 <- rsindex(td, freq = "quarterly", verbose = verbose, workers = workers)
  log_msg("Completed: quarterly_nuts1")
  quarterly_nuts2 <- rsindex(td, gclass = "nuts2", freq = "quarterly", verbose = verbose, workers = workers)
  log_msg("Completed: quarterly_nuts2")
  quarterly_nuts3 <- rsindex(td, gclass = "nuts3", freq = "quarterly", verbose = verbose, workers = workers)
  log_msg("Completed: quarterly_nuts3")

  if (save) {
    write_data(quarterly_aggregate, quarterly_nuts1, quarterly_nuts2, quarterly_nuts3, release = release_name)
  }

  list(aggregate = quarterly_aggregate, nuts1 = quarterly_nuts1, nuts2 = quarterly_nuts2, nuts3 = quarterly_nuts3)
}

#' @rdname update
#' @export
update_annual <- function(td, release_name = next_release(), save = TRUE, verbose = FALSE, workers = 1) {

  if(is.null(release_name)) {
    stop("you have to provide a `release_date`", call. = FALSE)
  }
  if (save && release_files_exist(release_name, "annual")) {
    log_msg("annual", release_name, "already written, skipping")
    return(invisible(NULL))
  }

  annual_uk <- rsindex(td, gclass = "uk", freq = "annual", verbose = verbose, workers = workers)
  log_msg("Completed: annual_uk")
  annual_countries <- rsindex(td, gclass = "countries", freq = "annual", verbose = verbose, workers = workers)
  log_msg("Completed: annual_countries")
  annual_london <- rsindex(td, gclass = "london_effect", freq = "annual", verbose = verbose, workers = workers)
  log_msg("Completed: annual_london")

  annual_aggregate <- reduce_join(annual_uk, annual_countries, annual_london)
  log_msg("Completed: annual_aggregate")
  annual_nuts1 <- rsindex(td, freq = "annual", verbose = verbose, workers = workers)
  log_msg("Completed: annual_nuts1")
  annual_nuts2 <- rsindex(td, gclass = "nuts2", freq = "annual", verbose = verbose, workers = workers)
  log_msg("Completed: annual_nuts2")
  annual_nuts3 <- rsindex(td, gclass = "nuts3", freq = "annual", verbose = verbose, workers = workers)
  log_msg("Completed: annual_nuts3")

  if (save) {
    write_data(annual_aggregate, annual_nuts1, annual_nuts2, annual_nuts3, release = release_name)
  }

  list(aggregate = annual_aggregate, nuts1 = annual_nuts1, nuts2 = annual_nuts2, nuts3 = annual_nuts3)
}
