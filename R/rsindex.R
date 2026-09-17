

# Calculate the repeated-sales index --------------------------------------

# data <- td
# gclass = "london_effect"
# freq = "monthly"
# period_trans = 100
# ntras_low = 1
# ntrans_high = 8
# abs_annual_ret = 0.15

#' Calculate the House Price Index using the Repeated Sales methodology
#' 
#' @param data The data to use
#' @param gclass geographical classification
#' @param freq frequenct
#' @param period_trans minimum period of translations
#' @param ntras_low minimum number of translations
#' @param ntrans_high maximum number of translations
#' @param abs_annual_ret should not exceed the absolute annual return 
#' 
#' @importFrom lubridate year
#' @importFrom purrr map map2 compact reduce map_lgl map_dbl
#' @importFrom tibble tibble as_tibble add_column
#' @importFrom dplyr bind_cols select everything
#' @importFrom tidyr gather
#' @importFrom zoo as.yearmon as.yearqtr coredata zoo
#' @importFrom Matrix sparseMatrix solve crossprod tcrossprod t
#' @importFrom ISOweek ISOweek2date
#' @importFrom progress progress_bar
#' @importFrom data.table data.table .GRP .SD .N key setorder setkeyv .SD shift
#' @param verbose print per-region timing (data prep, sparse matrix build, each solve stage).
#' @param workers number of regions to process in parallel (1 = sequential, the default).
#'   Each worker only receives its own region's data (not the full `data`), so memory use
#'   scales with dataset size, not with `workers`. Only helps when a `gclass` has many
#'   regions (nuts2/nuts3); does nothing for single-region classes like "uk".

# Top-level (not inline) date converters: their enclosing environment is the package
# namespace, not an rsindex() call frame, so passing them to a worker never drags `data` along.
.rsindex_date_daily <- function(x) as.Date(x, format = "%Y-%m-%d")
.rsindex_date_monthly <- function(x) zoo::as.yearmon(x, format = "%Y-%m-%d")
.rsindex_date_quarterly <- function(x) zoo::as.yearqtr(x, format = "%Y-%m-%d")

# Builds the per-region estimator as a closure over ONLY these small scalars/functions -
# never over `data`/`data_split` - so shipping it to a parallel worker never ships the dataset.
make_region_processor <- function(freq, den, date_conv, period_trans, ntras_low, ntrans_high,
                                   time_diff, date_scalar_idx, date_scalar_absret, abs_annual_ret,
                                   verbose) {
  # force args now: unforced promises still reference rsindex()'s calling frame (where
  # `data`/`data_split` live), so without this the closure would leak the dataset anyway
  force(freq); force(den); force(date_conv); force(period_trans); force(ntras_low)
  force(ntrans_high); force(time_diff); force(date_scalar_idx); force(date_scalar_absret)
  force(abs_annual_ret); force(verbose)
  function(ed, region_name = NA_character_) {
    t_region <- proc.time()

    ### CREATE Dates Using the Zoo function as.yearmon or as.yearqtr - as specified in the beginning of the file
    if (freq == "weekly") {
      ed[,WeekAux := paste(ISOweek(Date),"-3",sep = "")]; # Transform all days to Wednesdays
      ed[,Period := ISOweek2date(WeekAux) ];
    }else{
      ed[, Period := date_conv(Date)]
    }

    # Counting Transactions per time period, and if there are less than X (say 1000) transactions remove the time period
    counts <- ed[, .(rowCount = .N), by = Period]
    counts <- counts[rowCount > period_trans] # was 1000
    ed <- ed[(Period %in% counts$Period)]

    ############################################################################################
    # Drop columns to reduce table dimension. Keeping only: price ,date, postcode, PAON and SAON
    ed <- ed[, c("bizday", "PPCategory") := NULL]

    # Set key by Postcode, PAON and SAON
    setkeyv(ed, c("Postcode", "PAON", "SAON"))
    ed[, i := .GRP, by = key(ed)]

    # Order by date
    setorder(ed, i, Period)

    # Removing properties with a single transaction or with more than 8 transactions
    ed[, ntrans := .N, by = "i"] # ntrans: number of transactions per property
    ed <- ed[(ntrans > ntras_low & ntrans < ntrans_high)]

    ########################### Pairing Transactions ######################

    ed[, c("Lag_Price", "Lag_Date", "Lag_Period") := shift(.SD, 1, NA, "lag"), .SDcols = c("Price", "Date", "Period"), by = i]

    ### Remove lines with NA or Date1==Date2
    dd <- ed[(Price != "NA" & Period != Lag_Period)]

    # Creating Index For Months and calculate the time difference between transactions in months

    # Start Date
    start_date <- min(dd$Period)
    dd[, Time1index := date_scalar_idx * as.numeric(dd$Lag_Period - start_date)/den + 1]
    dd[, Time2index := date_scalar_idx * as.numeric(dd$Period - start_date)/den + 1]
    dd[, TimeDiff := Time2index - Time1index]

    # Remove Entries with Time Difference less than 180days/26weeks/6months and with Absolute Annual Returns higher than 15%
    dd <- dd[TimeDiff > time_diff]
    dd[, an_ret := (log(Price) - log(Lag_Price)) * date_scalar_absret / TimeDiff]
    dd <- dd[(abs(an_ret) <= abs_annual_ret)]

    # Create "Continuous" Day Indices from unique days
    ntime <- data.table(time = unique(c(dd$Time1index, dd$Time2index)))
    setorder(ntime, time) # order them
    ntime[, timeid := .GRP, by = "time"] # assign indexes
    dd[, time1id := ntime$timeid[match(Time1index, ntime$time)]]
    dd[, time2id := ntime$timeid[match(Time2index, ntime$time)]]

    # Estimation of House Price Index
    timediff <- dd$TimeDiff
    dd[, y := log(Price) - log(Lag_Price)]
    Ntime <- max(dd$time2id) # number of explanatory variables = number of days minus one
    N <- nrow(dd)
    i <- c(1:N, 1:N)
    j <- as.numeric(c(dd$time1id, dd$time2id))
    x <- as.numeric(c(rep(-1, N), rep(1, N)))

    if (verbose) message(sprintf("[%s] N=%d Ntime=%d prep: %.2fs", region_name, N, Ntime, (proc.time() - t_region)[["elapsed"]]))

    # Catching error - in case matrix is singular
    tryCatch({
      t_stage <- proc.time()
      # Sparse X matrix creation and 3 stage least squares regression
      mm <- sparseMatrix(i = i, j = j, x = x, dims = c(N, Ntime))[, -1] # create sparse matrix
      if (verbose) message(sprintf("[%s] sparseMatrix build: %.2fs", region_name, (proc.time() - t_stage)[["elapsed"]]))

      t_stage <- proc.time()
      sparse.sol <- solve(crossprod(mm), crossprod(mm, dd$y)) # solve (X'X)^-1X'y to obtain coefficient vector
      if (verbose) message(sprintf("[%s] stage1 solve: %.2fs", region_name, (proc.time() - t_stage)[["elapsed"]]))

      error <- dd$y - tcrossprod(mm, t(sparse.sol)) # compute error=y-X'b
      error2 <- error * error # squared residuals from first stage regression
      beta_error <- solve(crossprod(cbind(1, timediff)), crossprod(cbind(1, timediff), error2)) # coefficients for second regression
      sq_fitted <- sqrt(tcrossprod(cbind(1, timediff), t(beta_error))) # compute square of fitted values for second regression
      w <- 1 / sq_fitted@x # heteroskedastic weight per observation

      t_stage <- proc.time()
      beta_third <- solve(crossprod(mm, mm * w), crossprod(mm, w * dd$y)) # weighted (X'WX)^-1X'Wy, avoids building an NxN diagonal matrix
      if (verbose) message(sprintf("[%s] stage3 solve: %.2fs", region_name, (proc.time() - t_stage)[["elapsed"]]))

      ########### Getting Dates
      un_dates <- sort(unique(c(dd$Lag_Period, dd$Period)))

      ###### Saving Prices
      log_prices <- zoo(as.vector(beta_third), un_dates[-1])
      if (verbose) message(sprintf("[%s] total: %.2fs", region_name, (proc.time() - t_region)[["elapsed"]]))
      exp(log_prices)
    },
    error = function(e) e,
    warning = function(w) w
    )
  }
}

rsindex <- function(data, gclass = c("nuts1", "nuts2", "nuts3", "countries", "uk", "london_effect"),
                    freq = c("monthly", "quarterly", "annual", "daily", "weekly"),
                    period_trans = 100, ntras_low = 1, ntrans_high = 8, abs_annual_ret = 0.15,
                    verbose = FALSE, workers = 1) {

  gclass <- match.arg(gclass)
  freq <- match.arg(freq)

  gareas <- unique(data[[gclass]])
  gnames_id <- switch(gclass, nuts1 = "nm", nuts2 = "nm2", nuts3 = "nm3", countries = "countries", uk = "uk",
                      london_effect = "london_effect")
  gnames <- unique(data[[gnames_id]])

  # Split once instead of re-scanning the full table for every region
  data_split <- split(data, by = gclass)[as.character(gareas)]


  # Select data frequency ---------------------------------------------------
  # Maybe consider having time_diff also at the monhtly level to exclude trans that appear shorter thatn 6 months
  if (freq == "daily") {
    date_conv <- .rsindex_date_daily
    date_scalar_idx <- 1
    date_scalar_absret <- 365
    time_diff <- 180
    den <- 1
  }else if (freq == "weekly") {
    date_conv <- NULL
    date_scalar_idx <- 1
    date_scalar_absret <- 52
    time_diff <- 26
    den <- 7 # denominator only for weeks
  }else if (freq == "monthly") {
    date_conv <- .rsindex_date_monthly
    date_scalar_idx <- date_scalar_absret <- 12
    time_diff <- 6
    den <- 1
  }else if (freq == "quarterly") {
    date_conv <- .rsindex_date_quarterly
    date_scalar_idx <- date_scalar_absret <- 4
    time_diff <- 2
    den <- 1
  }else if (freq == "annual") {
    date_conv <- lubridate::year
    date_scalar_idx <- date_scalar_absret <- 1
    time_diff <- 0
    den <- 1
  }

  # Estimate the repeated-sales index for a single region's transactions.
  # Built by a top-level factory (not a closure over this frame) so parallel workers
  # never receive `data`/`data_split` - only the small scalars this needs.
  process_region <- make_region_processor(freq, den, date_conv, period_trans, ntras_low, ntrans_high,
                                           time_diff, date_scalar_idx, date_scalar_absret, abs_annual_ret,
                                           verbose)

  workers <- max(1, min(workers, length(gareas), parallel::detectCores()))

  if (workers > 1) {
    cl <- parallel::makeCluster(workers)
    on.exit(parallel::stopCluster(cl), add = TRUE)
    # each worker only gets its own region's slice via clusterMap below, never the full `data`
    parallel::clusterEvalQ(cl, { library(data.table); library(Matrix); library(zoo); library(ISOweek); data.table::setDTthreads(1) })
    price_level <- suppressWarnings(
      parallel::clusterMap(cl, process_region, data_split, as.character(gareas), SIMPLIFY = FALSE)
    )
  } else {
    pb <- progress_bar$new(format = "[:bar] :current/:total (:percent) eta: :eta", width = 70, total = length(gareas))
    price_level <- vector("list", length(gareas))
    suppressWarnings({
      for (kk in seq_along(gareas)) {
        pb$tick()
        price_level[[kk]] <- process_region(data_split[[kk]], as.character(gareas[kk]))
      }
    })
  }

  is_null <- map_lgl(price_level, is.null)
  length_diff <- length(is_null) < length(gareas)
  if(length_diff > 0) {
    pad_false <- rep(length_diff)
    is_null <- c(is_null, pad_false)
  }
  new_names <- gnames[!is_null]
  names(price_level) <- new_names
  
  out_coredata <- price_level %>% 
    compact() %>% 
    map(coredata) 
  
  out <- out_coredata %>% 
    pad_uneven_cols() %>% 
    bind_cols() %>% 
    add_column(Date = idx_max(price_level)) %>% 
    select(Date, everything())
  
  structure(
    out,
    geo_class = gclass,
    geo_areas = gareas,
    geo_names = gnames,
    frequency = freq,
    names_keep = gnames[!is_null],
    names_drop = gnames[is_null]
  )
}

reduce_join <- function(x, y, z) {
  union_attrs <- purrr::map2(attributes(x), attributes(y), union) %>% 
    purrr::map2(attributes(z), union) %>% 
    map(~ .x[!is.na(.x)])
  out <- reduce(list(x, y, z), full_join, by = "Date")
  attributes(out) <- union_attrs
  out
}

idx_max <- function(x) {
  col_lengths <- map_dbl(x, length)
  col_num <- which.max(col_lengths)
  index(x[[col_num]])
}

pad_uneven_cols <- function(x) {
  col_lengths <- map_dbl(x, length)
  nmax <- max(col_lengths)
  npads <- nmax - col_lengths
  map2(x, npads, ~ c(.x, rep(NA, .y)))
}



