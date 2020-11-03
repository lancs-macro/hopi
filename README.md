
<!-- README.md is generated from README.Rmd. Please edit that file -->

# hopi

<!-- badges: start -->

<!-- badges: end -->

## CSV Data

if you want to download the data in CSV format you can see the latest
release in the [data
directory](https://github.com/lancs-macro/uk-house-prices/tree/master/data/).

| from       | to         | name    | released |
| :--------- | :--------- | :------ | :------- |
| 2020-01-01 | 2020-03-31 | 2020-Q1 | X        |
| 2020-04-01 | 2020-06-30 | 2020-Q2 | X        |
| 2020-07-01 | 2020-09-30 | 2020-Q3 | X        |
| 2020-10-01 | 2020-12-31 | 2020-Q4 |          |
| 2021-01-01 | 2021-03-31 | 2021-Q1 |          |
| 2021-04-01 | 2021-06-30 | 2021-Q2 |          |

# API

You can also take advantage of the static (read-only) API on github
through `githubusercontent`.

## Endpoint

`endpoint`:
<https://raw.githubusercontent.com/lancs-macro/hopi/master/data>

GET /{release}/{frequency}/{category}/

  - `release`: format %YYYY-Q%q (e.g. 2019-Q4).
  - `frequency`: one of “annual”, “quarterly” or “monthly” (TBA
    “weekly”, “daily”).
  - `category`: one of “nuts1”, “nuts2”, “nuts3”, “aggregate”, “all”.

GET /classifications.csv

  - overview of all the contents

## Fetch the data

### R

``` r
library(readr)

ukhp_get <- function(release = "2020-Q3", frequency = "monthly", classification = "nuts1") {
  endpoint <- "https://raw.githubusercontent.com/lancs-macro/hopi/master/data"
  query <- paste(endpoint, release, frequency, paste0(classification, ".csv"), sep = "/")
  readr::read_csv(query)
} 
ukhp_get()
#> Parsed with column specification:
#> cols(
#>   Date = col_character(),
#>   `East of England` = col_double(),
#>   `West Midlands (England)` = col_double(),
#>   `South West (England)` = col_double(),
#>   `North West (England)` = col_double(),
#>   `Yorkshire and The Humber` = col_double(),
#>   `South East (England)` = col_double(),
#>   London = col_double(),
#>   `North East (England)` = col_double(),
#>   Wales = col_double(),
#>   `East Midlands (England)` = col_double()
#> )
#> # A tibble: 308 x 11
#>    Date  `East of Englan~ `West Midlands ~ `South West (En~ `North West (En~
#>    <chr>            <dbl>            <dbl>            <dbl>            <dbl>
#>  1 Feb ~             1.02            1.01              1.01            1.01 
#>  2 Mar ~             1.02            1.01              1.01            1.02 
#>  3 Apr ~             1.01            0.994             1.01            1.01 
#>  4 May ~             1.02            1.00              1.01            1.02 
#>  5 Jun ~             1.02            1.01              1.02            1.01 
#>  6 Jul ~             1.02            1.00              1.00            0.999
#>  7 Aug ~             1.01            0.994             1.01            1.00 
#>  8 Sep ~             1.01            0.996             1.01            1.00 
#>  9 Oct ~             1.01            1.01              1.00            0.994
#> 10 Nov ~             1.02            1.01              1.00            1.00 
#> # ... with 298 more rows, and 6 more variables: `Yorkshire and The
#> #   Humber` <dbl>, `South East (England)` <dbl>, London <dbl>, `North East
#> #   (England)` <dbl>, Wales <dbl>, `East Midlands (England)` <dbl>
```

### Python

``` python

from pandas import pd

def ukhp_get(release = "latest", frequency = "monthly", classification = "nuts1"):
  endpoint = "https://lancs-macro.github.io/uk-house-prices"
  query_elements = [endpoint, release, frequency, classification + ".json"]
  query = "/".join(query_elements)
  print(pd.read_csv(query))
  
ukhp_get()
```

## Update

1.  Clone the repo

`git clone https://github.com/lancs-macro/hopi.git`

2.  Update

<!-- end list -->

``` r
devtools::load_all(".")

td <- process_data(end_date = next_release_to())
update(td, release_name = next_release())
```

<!--
# Preprocess --------------------------------------------------------------

# download_lr_file()
# tidy_lr_file()

# nuts1_weekly <- rsindex(td, freq = "weekly")
# nuts1_daily <- rsindex(td, freq = "daily")

# cleanup()
-->
