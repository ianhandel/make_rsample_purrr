Import data
================
ian Handel
2019-02-05

``` r
library(tidyverse)
```

    ## ── Attaching packages ──────────────────────────────────────────────────────────── tidyverse 1.2.1 ──

    ## ✔ ggplot2 3.1.0           ✔ purrr   0.3.0      
    ## ✔ tibble  2.0.99.9000     ✔ dplyr   0.7.8      
    ## ✔ tidyr   0.8.2           ✔ stringr 1.3.1      
    ## ✔ readr   1.3.1           ✔ forcats 0.3.0

    ## ── Conflicts ─────────────────────────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()

``` r
library(here)
```

    ## here() starts at /Users/ihandel/Documents/EDINBURGH/projects/make_rsample_purrr

``` r
# import data and make names and first column all lower case

dat <- read_csv(here("data", "raw", "mpg_data.csv")) %>% 
  set_names(nm = tolower(names(.))) %>% 
  mutate(manufacturer = tolower(manufacturer))
```

    ## Parsed with column specification:
    ## cols(
    ##   manufacturer = col_character(),
    ##   Model = col_character(),
    ##   displ = col_double(),
    ##   Year = col_double(),
    ##   cyl = col_double(),
    ##   trans = col_character(),
    ##   drv = col_character(),
    ##   cty = col_double(),
    ##   hwy = col_double(),
    ##   fl = col_character(),
    ##   class = col_character()
    ## )

``` r
# write out

write_csv(dat, here("data", "clean", "mpg_data_clean.csv"))
```
