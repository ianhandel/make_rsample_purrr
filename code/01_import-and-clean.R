#' ---
#' title: "Import data"
#' author: "ian Handel"
#' date: "2019-02-05"
#' output: github_document
#' ---


library(readr)
library(here)

# import data and make names and first column all lower case

dat <- read_csv(here("data", "raw", "mpg_data.csv")) %>% 
  set_names(nm = tolower(names(.))) %>% 
  mutate(manufacturer = tolower(manufacturer))

# write out

write_csv(dat, here("data", "clean", "mpg_data_clean.csv"))
