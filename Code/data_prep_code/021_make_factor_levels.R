# 021_make_factor_levels.R
# Michael Boerman Jan 2021
#
# Turn the categorical variables into numbers
# This is a data-processing script.
#
# output: cat_data_reordered.csv
#         summary_df.csv
#
# ---- Setup ------------------------------------------------------------------#

library(readr)
library(dplyr)
library(tidyr)
library(here)

options(dplyr.summarise.inform = FALSE) # https://www.tidyverse.org/blog/2020/05/dplyr-1-0-0-last-minute-additions/; could add .groups = "drop_last" to each summarize() call.

# ---- Load Dummy Data --------------------------------------------------------#
# From 010_compile_kff_data:
data_lockdown_dummies <- read_csv(
  here("Intermediate_Data/data_lockdown_dummies.csv"),
  col_types = cols(
    .default = col_factor(),     # coerce to be factors instead of chr
    Date = col_date(format = "") # except column Date, read as date.
  )
)

# ---- Recode into numbers-------------- --------------------------------------#

# First, reorder by strictness / severity
#  (lower is less severe)
#  (No info = NA)

cat_data_reordered <- data_lockdown_dummies %>%
  # reorder and then assign values.

  # column 3
  mutate(Stay_Home_Order = factor(Stay_Home_Order,
    levels = c("-", "Lifted", "High-Risk Groups", "Curfew", "Certain Regions", "Statewide"),
    labels = c(0, 1, 2, 3, 4, 5)
  )) %>%


  # column 4.
  mutate(Status_of_Reopening = factor(Status_of_Reopening,
    levels = c("Reopened", "Paused", "Closing"),
    labels = c(1, 2, 3)
  )) %>%

  # column 5.
  mutate(NonEss_Business_Closed = factor(NonEss_Business_Closed,
    levels = c("-", "Other", "Some Reopened", "Reduced Capacity", "Some Closed", "All Non-Essential Retail Businesses Closed", "All Non-Essential Businesses Closed"),
    labels = c(0, 0, 1, 2, 3, 4, 5)
  )) %>%

  # column 6.
  mutate(Gathering_Limit = factor(Gathering_Limit,
    levels = c("-", "Other", "No Limit", "50", "25", "20", "10", "5", "All Prohbited"),
    labels = c(0, 0, 1, 2, 3, 4, 5, 6, 7)
  )) %>%

  # column 7. Only one factor level present.
  mutate(Emergency_Declaration = factor(Emergency_Declaration,
    levels = "Yes",
    labels = c(1)
  )) %>%

  # column 8.
  mutate(Travel_Quarantine = factor(Travel_Quarantine,
    levels = c("-", "Other", "No Restrictions", "International", "Air Only", "Certain States", "All Travelers"),
    labels = c(0, 0, 1, 2, 3, 4, 5)
  )) %>%

  # column 9.
  mutate(Mask_Requirement = factor(Mask_Requirement,
    levels = c("-", "No", "Certain Employees Only", "General Public", "Yes"),
    labels = c(0, 1, 2, 3, 3)
  )) %>%

  # column 10.
  mutate(Restaurants = factor(Restaurants,
    levels = c("-", "No Limits", "Limited Indoor", "No Indoor"),
    labels = c(0, 1, 2, 3)
  )) %>%

  # column 11.
  mutate(School_Closed = factor(School_Closed,
    levels = c("-", "Reccomended", "Closed"),
    labels = c(0, 1, 2)
  )) %>%
  identity()


# ---- Summarize --------------------------------------------------------------#

# Turn into numeric instead of factors. then scale each.
# Skip the first two columsn, which are date and location.
cat_data_reordered[, -c(1:2)] <- apply(cat_data_reordered[-c(1:2)], 2, as.numeric)
write_csv(cat_data_reordered, here("Intermediate_Data", "cat_data_reordered.csv"))

# Now summarize by aggregating across states (national index)
summary_df <- cat_data_reordered %>%
  group_by(Date) %>%
  summarize(across(where(is.numeric), sum, na.rm = TRUE)) %>% 
  ungroup() %>%
  pivot_longer(cols = where(is.numeric), names_to = "Category", values_to = "sums") %>%
  write_csv(here("Intermediate_Data", "summary_df.csv"))
