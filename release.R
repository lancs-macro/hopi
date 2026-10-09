# Release script - run from the hopi/ package root (e.g. `Rscript release.R`,
# or source it line by line in RStudio as before).
#
# Differences from the old ad-hoc version:
#  - skips the download entirely if temp/main.csv already exists (never re-downloads
#    over a file you already spent hours fetching)
#  - caches `td` to temp/td_cache.rds right after process_data(), so a crash in
#    update_quarterly()/update_annual() doesn't force re-reading+re-filtering the
#    whole multi-GB CSV again
#  - update_monthly()/update_quarterly()/update_annual() now skip recomputation
#    entirely if that release's CSVs are already on disk (see release_files_exist()
#    in R/update.R), so re-running this script after a partial failure only redoes
#    the stage(s) that didn't finish
#  - every stage is wrapped in tryCatch and logged with a timestamp to both the
#    console and release.log, so an overnight/multi-day run leaves a readable trail
#  - main.csv is NEVER deleted automatically - see the note at the end
#  - writes data/<release>/release.txt: data coverage, run timing, skipped/failed
#    regions, and a diff against the previous release (new rows, revised values,
#    column changes) for every frequency/type combination

devtools::load_all(".")
options(timeout = 60 * 60)

log_file <- "release.log"
log_line <- function(...) {
  msg <- paste0(format(Sys.time(), "[%Y-%m-%d %H:%M:%S] "), ...)
  cat(msg, "\n")
  cat(msg, "\n", file = log_file, append = TRUE)
}

# Set >1 to parallelize rsindex() across regions (helps nuts2/nuts3, not uk/countries).
# Set verbose = TRUE for per-region timing breakdowns.
workers <- 1
verbose <- FALSE

# 1. Get the data file --------------------------------------------------------
csv_path <- "temp/main.csv"

if (fs::file_exists(csv_path)) {
  log_line("Using existing ", csv_path, " (skipping download)")
} else {
  log_line("Downloading land registry file...")
  csv_path <- dowload_file()
  log_line("Download complete: ", csv_path)
}

# 2. Process (with caching) ----------------------------------------------------
end_date <- next_release_to_date()
td_cache <- "temp/td_cache.rds"

td <- NULL
if (
  fs::file_exists(td_cache) &&
    fs::file_info(td_cache)$modification_time > fs::file_info(csv_path)$modification_time
) {
  log_line("Loading cached td from ", td_cache)
  td <- tryCatch(readRDS(td_cache), error = function(e) NULL)
}

if (is.null(td)) {
  log_line("Running process_data() on ", csv_path, " (end_date = ", as.character(end_date), ")")
  td <- process_data(csv_path, end_date = end_date)
  saveRDS(td, td_cache)
  log_line("process_data() done, ", nrow(td), " rows, cached to ", td_cache)
}
gc()

nr <- next_release()
log_line("Release: ", nr)

# 3. Run each frequency, independently, without losing prior progress on failure ---
# Every message() raised inside (region skipped/failed diagnostics from rsindex())
# gets tee'd into release.log too, so the report step below can find them - without
# this they'd only ever show up on the console.
run_stage <- function(name, fn) {
  log_line("Starting: ", name)
  result <- withCallingHandlers(
    tryCatch(
      fn(),
      error = function(e) {
        log_line("FAILED: ", name, " - ", conditionMessage(e))
        NULL
      }
    ),
    message = function(m) {
      cat(m$message, file = log_file, append = TRUE)
    }
  )
  if (!is.null(result)) {
    log_line("Finished: ", name)
  }
  result
}

run_start <- Sys.time()
run_stage("monthly", function() update_monthly(td, nr, verbose = verbose, workers = workers))
run_stage("quarterly", function() update_quarterly(td, nr, verbose = verbose, workers = workers))
run_stage("annual", function() update_annual(td, nr, verbose = verbose, workers = workers))
run_end <- Sys.time()

gc()
log_line("Release run finished.")

# 4. Release report -------------------------------------------------------------
# Compares this release's output against the most recent previously-released
# quarter (whatever's actually sitting in data/, not an assumed prior quarter).
write_release_report <- function(release, run_start, run_end, td, log_file) {
  release_dir <- file.path("data", release)

  existing <- setdiff(list.dirs("data", recursive = FALSE, full.names = FALSE), release)
  existing <- sort(existing[grepl("^\\d{4}-Q[1-4]$", existing)])
  prev_candidates <- existing[existing < release]
  prev_release <- if (length(prev_candidates) > 0) tail(prev_candidates, 1) else NA_character_

  data_start <- substr(min(td$Date), 1, 10)
  data_end <- substr(max(td$Date), 1, 10)

  log_lines <- if (fs::file_exists(log_file)) readLines(log_file) else character(0)
  skipped <- grep("no valid repeat-sales pairs", log_lines, value = TRUE)
  failed <- grep("\\] failed:", log_lines, value = TRUE)

  compare_one <- function(freq, type, prev_dir) {
    old_path <- file.path(prev_dir, freq, paste0(type, ".csv"))
    new_path <- file.path(release_dir, freq, paste0(type, ".csv"))
    if (!fs::file_exists(old_path) || !fs::file_exists(new_path)) {
      return(sprintf(
        "  %s/%s.csv: missing (old=%s, new=%s)",
        freq,
        type,
        fs::file_exists(old_path),
        fs::file_exists(new_path)
      ))
    }

    old <- data.table::fread(old_path, colClasses = "character")
    new <- data.table::fread(new_path, colClasses = "character")

    added_cols <- setdiff(names(new), names(old))
    removed_cols <- setdiff(names(old), names(new))
    common_cols <- setdiff(intersect(names(old), names(new)), "Date")

    new_rows <- setdiff(new$Date, old$Date)
    common_dates <- intersect(old$Date, new$Date)

    n_revised <- 0L
    max_diff <- NA_real_
    if (length(common_dates) > 0 && length(common_cols) > 0) {
      old_m <- as.matrix(old[match(common_dates, old$Date), ..common_cols])
      new_m <- as.matrix(new[match(common_dates, new$Date), ..common_cols])
      storage.mode(old_m) <- "double"
      storage.mode(new_m) <- "double"
      diffs <- abs(old_m - new_m)
      n_revised <- sum(diffs > 1e-6, na.rm = TRUE)
      if (any(!is.na(diffs))) max_diff <- max(diffs, na.rm = TRUE)
    }

    line1 <- sprintf(
      "  %s/%s.csv: %d new row(s)%s, %d revised value(s) among %d shared dates%s",
      freq,
      type,
      length(new_rows),
      if (length(new_rows) > 0) paste0(" (", paste(new_rows, collapse = ", "), ")") else "",
      n_revised,
      length(common_dates),
      if (n_revised > 0) sprintf(" (max abs revision: %.4f)", max_diff) else ""
    )
    extra <- character(0)
    if (length(added_cols) > 0) {
      extra <- c(extra, sprintf("    + columns added: %s", paste(added_cols, collapse = ", ")))
    }
    if (length(removed_cols) > 0) {
      extra <- c(extra, sprintf("    - columns removed: %s", paste(removed_cols, collapse = ", ")))
    }
    c(line1, extra)
  }

  freqs <- c("monthly", "quarterly", "annual")
  types <- c("aggregate", "nuts1", "nuts2", "nuts3")

  comparison_lines <- if (is.na(prev_release)) {
    "  (no previous release found in data/ to compare against)"
  } else {
    unlist(lapply(freqs, function(f) {
      c(sprintf("%s:", f), unlist(lapply(types, compare_one, freq = f, prev_dir = file.path("data", prev_release))))
    }))
  }

  elapsed_sec <- as.numeric(difftime(run_end, run_start, units = "secs"))

  report <- c(
    sprintf("HOPI Release Report: %s", release),
    sprintf("Generated: %s", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
    "",
    "Data coverage:",
    sprintf("  Earliest transaction date: %s", data_start),
    sprintf("  Latest transaction date:   %s", data_end),
    sprintf("  Transactions used:         %s", format(nrow(td), big.mark = ",")),
    "",
    "Run timing:",
    sprintf("  Started:  %s", format(run_start, "%Y-%m-%d %H:%M:%S")),
    sprintf("  Finished: %s", format(run_end, "%Y-%m-%d %H:%M:%S")),
    sprintf("  Elapsed:  %.0f seconds (%.1f minutes)", elapsed_sec, elapsed_sec / 60),
    "",
    sprintf("Regions skipped (no valid repeat-sales pairs): %d", length(skipped)),
    if (length(skipped) > 0) paste0("  ", skipped) else NULL,
    "",
    sprintf("Regions failed (regression error, excluded from output): %d", length(failed)),
    if (length(failed) > 0) paste0("  ", failed) else NULL,
    "",
    if (is.na(prev_release)) {
      "No previous release found to compare against."
    } else {
      sprintf("Comparison to previous release (%s):", prev_release)
    },
    comparison_lines
  )

  out_path <- file.path(release_dir, "release.txt")
  writeLines(report, out_path)
  log_line("Release report written to ", out_path)
}

write_release_report(nr, run_start, run_end, td, log_file)

# 5. Cleanup - deliberately NOT automatic. main.csv takes a long time to fetch;
# delete it yourself once you've checked the release looks right:
#   remove_file("temp/main.csv")
#   fs::file_delete("temp/td_cache.rds")
log_line("main.csv and td_cache.rds were left in place - remove them manually once you've verified this release.")
