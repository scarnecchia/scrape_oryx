*Local `.Rprofile` detected at `/mnt/d/Github/scrape_oryx/.Rprofile`*

``` r
library(rvest)
library(dplyr)
#> 
#> Attaching package: 'dplyr'
#> The following objects are masked from 'package:stats':
#> 
#>     filter, lag
#> The following objects are masked from 'package:base':
#> 
#>     intersect, setdiff, setequal, union
library(tidyr)
library(lubridate)
#> 
#> Attaching package: 'lubridate'
#> The following objects are masked from 'package:base':
#> 
#>     date, intersect, setdiff, union
library(purrr)
library(magrittr)
#> 
#> Attaching package: 'magrittr'
#> The following object is masked from 'package:purrr':
#> 
#>     set_names
#> The following object is masked from 'package:tidyr':
#> 
#>     extract
library(tibble)
library(stringr)
library(readr)
#> 
#> Attaching package: 'readr'
#> The following object is masked from 'package:rvest':
#> 
#>     guess_encoding
library(glue)
library(logr)
library(ggplot2)
library(scales)
#> 
#> Attaching package: 'scales'
#> The following object is masked from 'package:readr':
#> 
#>     col_factor
#> The following object is masked from 'package:purrr':
#> 
#>     discard
library(ggthemes)
library(fs)

get_inputfile <- function(.file) {
  path <- fs::dir_info("inputfiles", type = "file", regexp=".file") %>%
    dplyr::filter(!stringr::str_detect(path, ".bak")) %>%
    dplyr::select(path, change_time, birth_time) %>%
    dplyr::filter(stringr::str_detect(path, .file)) %>%
    dplyr::filter(birth_time == max(birth_time)) %>%
    dplyr::pull(path)

  message(path)

  # logr::put(path)

  readr::read_csv(path)
}

get_inputfile(.file="totals_by_system")
#> inputfiles/totals_by_system2022-04-08.csvinputfiles/totals_by_system2022-04-09.csvinputfiles/totals_by_system2022-04-10.csvinputfiles/totals_by_system2022-04-11.csvinputfiles/totals_by_system2022-04-12.csvinputfiles/totals_by_system2022-04-14.csvinputfiles/totals_by_system2022-04-15.csvinputfiles/totals_by_system2022-04-16.csvinputfiles/totals_by_system2022-04-17.csvinputfiles/totals_by_system2022-04-18.csvinputfiles/totals_by_system2022-04-19.csvinputfiles/totals_by_system2022-04-20.csvinputfiles/totals_by_system2022-04-21.csvinputfiles/totals_by_system2022-04-22.csvinputfiles/totals_by_system2022-04-23.csvinputfiles/totals_by_system2022-04-24.csvinputfiles/totals_by_system2022-04-25.csvinputfiles/totals_by_system2022-04-26.csvinputfiles/totals_by_system2022-05-02.csvinputfiles/totals_by_system2022-05-04.csvinputfiles/totals_by_system2022-05-06.csv
#> Error: Files must all have 6 columns:
#> * File 20 has 7 columns
```

<sup>Created on 2022-05-08 by the [reprex package](https://reprex.tidyverse.org) (v2.0.1)</sup>
