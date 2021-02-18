# 012_gather_populations.R
# Michael Boerman January 2020
#
# A simple script to get the latest estimates of state populations
#
# Source: https://www.census.gov/data/datasets/time-series/demo/popest/2010s-state-total.html
# Table: "Annual Estimates of the Resident Population for the United States, Regions, States, and Puerto Rico: April 1, 2010 to July 1, 2019 (NST-EST2019-01)"
#
# Link: https://www2.census.gov/programs-surveys/popest/datasets/2010-2020/national/totals/nst-est2020.csv
# (~18kb)
#
# Data updated last on Jan 14, 2020.
# ---- SETUP ------------------------------------------------------------------#

library(readr)
library(here)
library(dplyr)

# ---- READ -------------------------------------------------------------------#

census_url <- "https://www2.census.gov/programs-surveys/popest/datasets/2010-2020/national/totals/nst-est2020.csv"

download.file(
  url = census_url,
  destfile = here("Data/census_data.csv"),
  # method = "wget"
)

state_populations <- read_csv(here("Data/census_data.csv")) %>%
  filter(NAME %in% state.name | NAME %in% "District of Columbia") %>% # this is how I discovered DC is not a state
  select(NAME, POPESTIMATE2020) %>%
  rename(
    state = NAME,
    population = POPESTIMATE2020
  ) %>%
  write_csv(here("Intermediate_Data/state_populations.csv"))



# Read in manually in case URL changes in the future. Estimates won't be changing any time soon, but I have spent too much of my life replicating depreciated urls.
# state_populations <- read_csv(here("Data/nst-est2020.csv"), col_names = TRUE) %>%
#   filter(NAME %in% state.name | NAME %in% "District of Columbia") %>%
#   select(NAME, POPESTIMATE2020) %>%
#   rename(
#     state = NAME,
#     population = POPESTIMATE2020
#   ) %>%
#   write_csv(here("Intermediate_Data/state_populations.csv"))
