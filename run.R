
# Download and clean the raw file -----------------------------------------

# These functions are only needed for update 

download_lr_file()

tidy_lr_file()

# Cleaning data -----------------------------------------------------------

dt <- tidy_data()

# Creating indices --------------------------------------------------------

uk_monthly <- rsindex(dt, gclass = "uk", freq = "monthly")
countries_monthly <- rsindex(dt, gclass = "countries", freq = "monthly")

nuts1_monthly <- rsindex(dt) #works
nuts2_monthly <- rsindex(dt, gclass = "nuts2") # works
nuts3_monthly <- rsindex(dt, gclass = "nuts3") # works

nuts1_quarterly <- rsindex(dt, freq = "quarterly") # works
nuts2_quarterly <- rsindex(dt, gclass = "nuts2", freq = "quarterly") # works
nuts3_quarterly <- rsindex(dt, gclass = "nuts3", freq = "quarterly") # works

nuts1_annual <- rsindex(dt, freq = "annual") # works
nuts2_annual <- rsindex(dt, gclass = "nuts2", freq = "annual") # works
nuts3_annual <- rsindex(dt, gclass = "nuts3", freq = "annual") # works

# nuts1_weekly <- rsindex(dt, freq = "weekly")
# nuts1_daily <- rsindex(dt, freq = "daily")




cleanup()
