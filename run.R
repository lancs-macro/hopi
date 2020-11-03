

# Preprocess --------------------------------------------------------------

# download_lr_file()
# tidy_lr_file()

# Clean data --------------------------------------------------------------

td <- process_data(end_date = "2020-09-30")
all <- update(td, release_name = "2020-Q3")

# monthly --------------------------------------------------------------

monthly <- update_monthly(td, release_name = "2020-Q2")
quarterly <- update_quarterly(td, release_name = "2020-Q2")
annual <- update_annual(td, release_name = "2020-Q2")


# nuts1_weekly <- rsindex(td, freq = "weekly")
# nuts1_daily <- rsindex(td, freq = "daily")

# cleanup()



  
