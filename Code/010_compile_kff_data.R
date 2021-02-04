# 010_compile_kff_csv.R
# Michael Boerman Sept 2020
#
# Purpose: Read and clean vintage data on state's covid reponses
#
# Vintage data comes from
#
# 3/25/2020 - 12/20/2020:
#  the Way Back Machine's web archive of
#  https://www.kff.org/health-costs/issue-brief/state-data-and-policy-actions-to-address-coronavirus/
#  and are stored in /Data/kff_website using mm-dd-yyyy
#
# 1/04/2021 - ongoing:
#  The KFF's github page for the website
#  https://github.com/KFFData/COVID-19-Data/tree/kff_master/State%20Policy%20Actions/State%20Social%20Distancing%20Actions
#  and are stored in /Data/kff_github using mm-dd-yyyy
#
# The page is not updated every single day, only when there is a change in policy.
#
# I make the assumption that if there is no update to the website, then there
#   is no update to state policy.
#
# I also assume there is no lag, such that the date of the update corresponds to
#   the date of policy implementation.

# ----- SETUP ------------------------------------------------------------------
# Suppress some outputs
options(tidyverse.quiet = TRUE)

# Load libraries
library(here)
library(tidyverse)
library(lubridate)
library(parallel)

# ----- READ DATA --------------------------------------------------------------
# I manually saved each vintage from the archived site. The names of the files
# are the dates of the vintage.

files <- c(
  list.files(here("Data/kff_website")),
  list.files(here("Data/kff_github"))
)
dates <- mdy(str_extract(files, pattern = ".+?(?=\\.)")) # https://regex101.com/r/TOxdz8/1

# re-order according to date correctly (as is, Jan 2021 comes before March 2020).
dates_ordered <- dates[order(dates)]
files_ordered <- files[order(dates)]

# function to read in each csv, and create a column with the date.
read_vintage_csv <- function(file_name) {
  file_date <- mdy(str_extract(file_name, pattern = ".+?(?=\\.)")) # https://regex101.com/r/TOxdz8/1

  # if < the date of changeover from website to github,
  # then read from the kff_website folder and modify appropriately:
  if (file_date <= mdy("12-18-2020")) {
    data <- read_csv(
      file = here("Data/kff_website", file_name),
      skip = 2
    ) %>%
      filter(Location != "United States") %>% # remove US-wide stats
      head(n = 51) %>% # 50 states + DC
      select(-Footnotes) %>% # remove col listing the footnotes.
      mutate_if(is.character, as.factor) %>% # change to factor
      mutate(Date = file_date) %>%
      identity()
  } else {
    # else if >= 12-20-2021,
    # then do similar code but read from kff_github and different csv structure.
    data <- read_csv(file = here("Data/kff_github", file_name)) %>%
      rename(Location = X1) %>% # First col is un-named
      filter(Location != "United States") %>% # remove US-wide stats
      mutate_if(is.character, as.factor) %>% # change to factor
      mutate(Date = file_date) %>%
      identity()
  }
  return(data)
}

# Now apply this function to all files in the csv folder.
# Linux version:
# data <- mcMap(read_vintage_csv, files_ordered, mc.cores = detectCores())

# Windows version: takes a little longer. can't use all cores. 
data <- map(files_ordered, read_vintage_csv)

# change the names of the list items. (not functional / used later)
names(data) <- paste0(
  "vint_",
  substr(dates_ordered, start = 1, stop = 4), # get the year
  "-",
  substr(dates_ordered, start = 6, stop = 10) # get the day and month
)

# ---- EXPLORE ----------------------------------------------------------------#
# I'd like to look at the "original" data to see the number of levels in each category.
# This should not always be run.

# original <- plyr::join_all(dfs = data, type = "full")
# summary(original)

# colnames(original)[16]
# levels(original[,16])

# ----- CLEAN -----------------------------------------------------------------#
# Now tidy up the levels:
#  -Many categories are similar, so combine those.
#  -Many levels are similar, so combine those.
#  -Many stop being recorded, so drop those.
#  -Some are irrelevant (to me), so drop those.
#  -Many have spaces in their names, so _ those.

cleaned <- plyr::join_all(dfs = data, type = "full") %>%

  # Mand Quarentine was only for first few days, then changed name.
  select(-"Mandatory Quarantine") %>%

  # "State is Easing Social Distance Measures" is 9 levels, 1/5 are "yes" and rest
  # are "NA". Info is captured elsewhere, so remove this summary column.
  select(-"State Is Easing Social Distancing Measures") %>%

  # Remove this; not needed imo, and 4/5 is NA.
  select(-"Primary Election Postponement") %>%

  # The data changes a month in to split bars and restaurants.
  # merge these into one column by replacing NAs.
  mutate(Restaurants = case_when(
    !is.na(`Restaurant Limits`) ~ as.character(`Restaurant Limits`),
    is.na(`Restaurant Limits`) ~ as.character(`Bar/Restaurant Limits`)
  )) %>%
  select(-c(
    "Restaurant Limits", "Bar/Restaurant Limits",
    "Bar Closures", "Bar Closures*"
  )) %>%

  # Now clean up the Restaurant levels
  mutate(
    Restaurants =
      case_when(
        grepl("^-", Restaurants, fixed = FALSE) ~ "-",
        grepl("elivery", Restaurants, fixed = TRUE) | grepl("Closed", Restaurants, fixed = TRUE) ~ "Takeout/Delivery Only",
        grepl("Limit", Restaurants, fixed = TRUE) & grepl("Service", Restaurants, fixed = TRUE) ~ "Limited Dine-In",
        grepl("Reopened", Restaurants, fixed = TRUE) ~ "No Limits"
      )
  ) %>%

  # School closure series stops around July. Should exclude this or fill in for yes?
  mutate(School_Closed = coalesce(`State-Mandated School Closures`, `School Closures`)) %>%
  select(-c("State-Mandated School Closures", "School Closures")) %>%
  mutate(
    School_Closed =
      case_when(
        grepl("Closed", School_Closed, fixed = FALSE) ~ "Closed",
        grepl("Yes", School_Closed, fixed = FALSE) ~ "Closed",
        grepl("Recommended", School_Closed, fixed = FALSE) ~ "Recommended",
        grepl("Other", School_Closed, fixed = FALSE) ~ "-"
      )
  ) %>%

  # Gathering limit has 14 levels with duplicate info. boil down.
  dplyr::rename(Gathering_Limit = `Large Gatherings Ban`) %>%
  mutate(Gathering_Limit = case_when(
    grepl("10", Gathering_Limit, fixed = TRUE) ~ "10",
    grepl(">5 ", Gathering_Limit, fixed = TRUE) ~ "5",
    grepl("20", Gathering_Limit, fixed = TRUE) ~ "20",
    grepl("25", Gathering_Limit, fixed = TRUE) ~ "25",
    grepl("50", Gathering_Limit, fixed = TRUE) ~ "50",
    grepl("All", Gathering_Limit, fixed = TRUE) ~ "All Prohbited",
    grepl("Other", Gathering_Limit, fixed = TRUE) ~ "Other",
    grepl("Lifted", Gathering_Limit, fixed = TRUE) ~ "No Limit",
    grepl("No Limit", Gathering_Limit, fixed = TRUE) ~ "No Limit",
    grepl("-", Gathering_Limit, fixed = TRUE) ~ "-"
  )) %>%

  # Non-Essential Business Closures has 14 levels. Boil down.
  dplyr::rename(NonEss_Business_Closed = "Non-Essential Business Closures") %>%
  mutate(
    NonEss_Business_Closed =
      case_when(
        grepl("^-", NonEss_Business_Closed, fixed = FALSE) ~ "-",
        grepl("Other", NonEss_Business_Closed, fixed = TRUE) ~ "Other",
        grepl("All Non-Essential Businesses", NonEss_Business_Closed, fixed = FALSE) ~ "All Non-Essential Businesses Closed",
        grepl("All Non-Essential Retail Businesses", NonEss_Business_Closed, fixed = TRUE) ~ "All Non-Essential Retail Businesses Closed",
        grepl("Certain", NonEss_Business_Closed, fixed = TRUE) ~ "Some Closed",
        grepl("Some Non-Essential Businesses Closed", NonEss_Business_Closed, fixed = TRUE) ~ "Some Closed",
        grepl("Open with Reduced Capacity", NonEss_Business_Closed, fixed = FALSE) ~ "Reduced Capacity",
        grepl("All Non-Essential Businesses Permitted to Reopen with Reduced Capacity", NonEss_Business_Closed, fixed = TRUE) ~ "Reduced Capacity",
        grepl("Some Non-Essential Businesses Permitted to Reopen with Reduced Capacity", NonEss_Business_Closed, fixed = TRUE) ~ "Reduced Capacity",
        grepl("Some Non-Essential Businesses Permitted to Reopen", NonEss_Business_Closed, fixed = TRUE) ~ "Some Reopened",
        grepl("All Non-Essential Businesses Permitted to Reopen", NonEss_Business_Closed, fixed = TRUE) ~ "All Reopened"
      )
  ) %>%

  # Travel Quarantine has 9 levels. Boil down.
  dplyr::rename(Travel_Quarantine = "Mandatory Quarantine for Travelers") %>%
  mutate(
    Travel_Quarantine =
      case_when(
        grepl("^-", Travel_Quarantine, fixed = FALSE) ~ "-",
        grepl("Air Travelers", Travel_Quarantine, fixed = TRUE) ~ "Air Travelers",
        grepl("All Travelers", Travel_Quarantine, fixed = TRUE) ~ "All Travelers",
        grepl("Certain States", Travel_Quarantine, fixed = TRUE) ~ "Certain States",
        grepl("International", Travel_Quarantine, fixed = TRUE) ~ "International",
        grepl("Lifted", Travel_Quarantine, fixed = TRUE) ~ "No Restrictions",
        grepl("Other", Travel_Quarantine, fixed = TRUE) ~ "Other"
      )
  ) %>%

  # Two stay at home orders exist, one with capital At and one lowercase.
  mutate(Stay_Home_Order = case_when(
    !is.na(`Stay At Home Order`) ~ as.character(`Stay At Home Order`),
    !is.na(`Stay at Home Order`) ~ as.character(`Stay at Home Order`)
  )) %>%
  select(-c("Stay At Home Order", "Stay at Home Order")) %>%

  # clean up the categories
  mutate(
    Stay_Home_Order =
      case_when(
        grepl("^-", Stay_Home_Order, fixed = FALSE) ~ "-",
        grepl("High-Risk Groups", Stay_Home_Order, fixed = TRUE) ~ "High-Risk Groups",
        grepl("High-risk Groups", Stay_Home_Order, fixed = TRUE) ~ "High-Risk Groups",
        grepl("High Risk Groups", Stay_Home_Order, fixed = TRUE) ~ "High-Risk Groups",
        grepl("Lifted", Stay_Home_Order, fixed = TRUE) ~ "Lifted",
        grepl("Statewide", Stay_Home_Order, fixed = TRUE) ~ "Statewide"
      )
  ) %>%

  # Clean up face mask requirement levels
  dplyr::rename(Mask_Requirement = `Face Covering Requirement`) %>%
  mutate(
    Mask_Requirement =
      case_when(
        grepl("^-", Mask_Requirement, fixed = FALSE) ~ "-",
        grepl("General Public", Mask_Requirement, fixed = TRUE) ~ "General Public",
        grepl("Required for Certain Employees", Mask_Requirement, fixed = FALSE) ~ "Certain Employees Only"
      )
  ) %>%


  # Reorder some stuff
  relocate(Date, .after = Location) %>%
  relocate(`Status of Reopening`, .after = Date) %>%
  relocate(Stay_Home_Order, .after = Date) %>%

  # Rename some stuff to avoid spaces
  dplyr::rename(Emergency_Declaration = `Emergency Declaration`) %>%
  dplyr::rename(Status_of_Reopening = `Status of Reopening`) %>%
  mutate(
    Status_of_Reopening =
      case_when(
        grepl("Restrictions", Status_of_Reopening, fixed = TRUE) ~ "Closing",
        grepl("Paused", Status_of_Reopening, fixed = FALSE) ~ "Paused",
        grepl("Reopen", Status_of_Reopening, fixed = FALSE) ~ "Reopened"
      )
  ) %>%


  # change to factors
  mutate(
    Stay_Home_Order = as.factor(Stay_Home_Order),
    Status_of_Reopening = as.factor(Status_of_Reopening),
    NonEss_Business_Closed = as.factor(NonEss_Business_Closed),
    Gathering_Limit = as.factor(Gathering_Limit),
    Travel_Quarantine = as.factor(Travel_Quarantine),
    Mask_Requirement = as.factor(Mask_Requirement),
    Restaurants = as.factor(Restaurants),
    School_Closed = as.factor(School_Closed)
  ) %>%
  identity() # fin

# ---- INTERPOLATE ------------------------------------------------------------#
# Interpolate for days where there are no status changes.
# Use the previous day's status and increment the day by 1 and insert it.
#
# This could probably be done better by saving each chunk of 51 as a df in a list,
# and then using join_all to join them at once instead of doing it in a for loop.

cleaned_interpolated <- cleaned

for (i in 2:29000) { # the amount needed to loop through not just nrow(cleaned), but keep going when adding rows.
  if ((cleaned_interpolated$Date[i] - 1) > cleaned_interpolated$Date[i - 1]) { # day before current > previous date
    df <- cleaned_interpolated[(i - 51):(i - 1), ] # the last 51 obs
    df$Date <- cleaned_interpolated$Date[i - 1] + 1 # increase the day by 1.
    cleaned_interpolated <- full_join(cleaned_interpolated, df) %>% arrange(Date) # join with previous df and sort date.
  }
}

rm(list = c("df", "i"))

### Diagnose: Test to see if all days are sequential and such

# 1) some of these are from turn of month. not useful
which(cleaned$Date - 1 > lag(cleaned$Date))

# 2) if this is not 0, increase the for "i" end above. Important!!!!
# If this is 0, proceed.
which(cleaned_interpolated$Date - 1 > lag(cleaned_interpolated$Date))

# 3) should be equal in length. important.
length(unique(cleaned_interpolated$Date))
length(seq.Date(
  from = unique(cleaned_interpolated$Date[1]),
  to = unique(cleaned_interpolated$Date)[length(unique(cleaned_interpolated$Date))],
  by = 1
))

# ----- CONCLUSION ------------------------------------------------------------#
# Observe frequencies of each factor
# summary(cleaned_interpolated)

# Write to csv and Rdata
data_lockdown_dummies <- cleaned_interpolated
write_csv(data_lockdown_dummies, here("Intermediate_Data/data_lockdown_dummies.csv"))
