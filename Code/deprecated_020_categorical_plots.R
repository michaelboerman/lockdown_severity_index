# DEPRECATED! 
#   This large script has been broken into individual scripts:
#   - 021 to make the factors into numbers
#   - 032 to plot these raw numbers
#   - 035 to standardized the series and plot
#   - 037 to weigh by population and plot
#   - 038 to both standardize and weigh by population
#   
# Michael Boerman January 2021

# ---- Setup ------------------------------------------------------------------#
library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(here)

options(scipen = 999) # for the population-weighted scores
options(dplyr.summarise.inform = FALSE) # https://www.tidyverse.org/blog/2020/05/dplyr-1-0-0-last-minute-additions/; could add .groups = "drop_last" to each summarize() call.


# ---- Load Dummy Data --------------------------------------------------------#
# From 010_compile_kff_data:
data_lockdown_dummies <- read_csv(
  here("Intermediate_Data/data_lockdown_dummies.csv"),
  col_types = cols(
    .default = col_factor(), # coerce to be factors instead of chr
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
    levels = c("-", "Lifted", "High-Risk Groups", "Statewide"),
    labels = c(0, 1, 2, 3)
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
    levels = c("-", "Other", "No Limit", "5", "10", "20", "25", "50", "All Prohbited"),
    labels = c(0, 0, 1, 2, 3, 4, 5, 6, 7)
  )) %>%

  # column 7. Only one factor level present.
  mutate(Emergency_Declaration = factor(Emergency_Declaration,
    levels = "Yes",
    labels = c(1)
  )) %>%

  # column 8.
  mutate(Travel_Quarantine = factor(Travel_Quarantine,
    levels = c("-", "Other", "No Restrictions", "International", "Certain States", "Air Only", "All Locations"),
    labels = c(0, 0, 1, 2, 3, 4, 5)
  )) %>%

  # column 9.
  mutate(Mask_Requirement = factor(Mask_Requirement,
    levels = c("-", "Certain Employees Only", "General Public"),
    labels = c(0, 1, 2)
  )) %>%

  # column 10.
  mutate(Restaurants = factor(Restaurants,
    levels = c("-", "No Limits", "Limited Dine-In", "Takeout/Delivery Only"),
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

# Now summarize by aggregating across states (national index)
summary_df <- cat_data_reordered %>%
  group_by(Date) %>%
  summarize(
    Stay_Home_Order = sum(Stay_Home_Order, na.rm = T),
    Status_of_Reopening = sum(Status_of_Reopening, na.rm = T),
    NonEss_Business_Closed = sum(NonEss_Business_Closed, na.rm = T),
    Gathering_Limit = sum(Gathering_Limit, na.rm = T),
    Emergency_Declaration = sum(Emergency_Declaration, na.rm = T),
    Travel_Quarantine = sum(Travel_Quarantine, na.rm = T),
    Mask_Requirement = sum(Mask_Requirement, na.rm = T),
    Restaurants = sum(Restaurants, na.rm = T),
    School_Closed = sum(School_Closed, na.rm = T)
  ) %>%
  ungroup() %>%
  pivot_longer(cols = 2:10, names_to = "Category", values_to = "sums")

# Standardize them by subtracting mean and dividing by std dev.
standardized_scores <- summary_df %>%
  group_by(Category) %>%
  summarise(
    Date = Date,
    standardized_score = (sums - mean(sums, na.rm = T)) / sd(sums, na.rm = T)
  ) %>%
  ungroup() %>%
  group_by(Date) %>%
  summarize(composite_std_score = sum(standardized_score, na.rm = T)) %>%
  ungroup() %>%
  identity()

# ---- Plots ------------------------------------------------------------------#

# 1: All Categories (facet wrapped)
summary_df %>%
  ggplot(aes(x = Date, y = sums, color = Category)) +
  geom_line() +
  facet_wrap(~Category, scales = "fixed") + # fix to show the impact of each if not standardized.
  theme_minimal() +
  theme(
    axis.title.y = element_blank(),
    axis.title.x = element_blank(),
    legend.position = "none",
    aspect.ratio = .5
  ) +
  ggtitle(
    label = "United States Restrictions mapped to numbers",
    subtitle = "Low score means less severe restrictions."
  ) +
  labs(caption = paste0("Severity is calculated as the sum of 9 categorical variables mapped to numbers (dummy variables). NAs are counted as 0. \n Data is from ", min(weighted_std_scores$date), " through ", max(weighted_std_scores$date), ".")) +
  ggsave(here("Results/plots/categorical_facet.png"), width = 12, height = 6)

# 2. Aggregated into US
summary_df %>%
  group_by(Date) %>%
  summarize(total_sum = sum(sums, na.rm = T)) %>%
  ungroup() %>%
  ggplot(aes(x = Date, y = total_sum)) +
  geom_line(size = 1.25) +
  theme_minimal() +
  theme(
    panel.grid.major.x = element_line(size = 1, colour = "lightgrey"),
    panel.grid.minor.y = element_blank(),
    axis.title.x = element_blank(),
    aspect.ratio = .5
  ) +
  scale_y_continuous(position = "right") +
  scale_x_date(
    date_minor_breaks = "1 week",
    date_breaks = "1 month",
    date_labels = " %b \n %Y"
  ) +
  ggtitle(
    label = "US Rescriction Severity Index",
    subtitle = "Low score means less severe restrictions."
  ) +
  labs(caption = paste0("Severity is calculated as the sum of 9 categorical variables mapped to numbers (dummy variables). NAs are counted as 0. \n Data is from ", min(weighted_std_scores$date), " through ", max(weighted_std_scores$date), ".")) +
  ylab("Severity Index Score") +
  ggsave(here("Results/plots/categorical_aggregate.png"), width = 12, height = 6)

# 3. Standardized aggregated into US.
standardized_scores %>%
  ggplot(aes(x = Date, y = composite_std_score)) +
  geom_line(size = 1.25) +
  theme_minimal() +
  theme(
    panel.grid.major.x = element_line(size = 1, colour = "lightgrey"),
    panel.grid.minor.y = element_blank(),
    axis.title.x = element_blank(),
    aspect.ratio = .5
  ) +
  scale_y_continuous(position = "right") +
  scale_x_date(
    date_minor_breaks = "1 week",
    date_breaks = "1 month",
    date_labels = " %b \n %Y"
  ) +
  ggtitle(
    label = "US Rescriction Severity Index (Standardized)",
    subtitle = "Low score means less severe restrictions."
  ) +
  labs(caption = paste0("Standardized severity is calculated by standardizing (subtract mean and divid by standard deviation) the nationally-aggregated scores across 9 categorical variables. \n Standardization is necessary to correctly weight each category, regardless of the number of levels of it. \n Data is from ", min(standardized_scores$Date), " through ", max(standardized_scores$Date), ".")) +
  ylab("Severity Index Score") +
  ggsave(here("Results/plots/standardized_categorical_aggregate.png"), width = 12, height = 6)

# ----- POPULATION-WEIGHTED VERSION -------------------------------------------#


# ---- Load Population Data ---------------------------------------------------#
# Output of 012_gather_populations.R
state_pop <- read_csv(here("Intermediate_Data/state_populations.csv"))


# ---- Merge and Clean --------------------------------------------------------#

# Goal: need to change from full state names to be the abbreviations.
#   however, state.name and .abb don't contain DC.

# https://stackoverflow.com/questions/5411979/state-name-to-abbreviation solution #2 in first comment (vectorized)

# Note: The state populations appear static. thus, the national population is static, too.

# First, we create a dataframe that has the state populations included with the dummies.
dummies_with_pop <- cat_data_reordered %>%
  mutate(Location = as.character(Location)) %>%
  mutate(
    state = case_when(
      Location %in% state.name ~ setNames(state.abb, state.name)[Location],
      Location == "District of Columbia" ~ "DC",
      TRUE ~ "Error!"
    ),
    .after = Date
  ) %>%

  # this is sketchy. repeating the state_pop instead of matchig it over. merge wasn't working.
  mutate(
    population = rep(state_pop$population, times = nrow(cat_data_reordered) / 51),
    .after = state
  ) %>%
  rename(date = Date) %>%
  select(-Location) %>%
  identity()


# ---- Weight by population ---------------------------------------------------#
# Next, we'll group by the date and the state to come up with sum of dummies.
# Basically want one score instead of the 9 individual components.

# weighted but not standardized
weighted_scores <- dummies_with_pop %>%
  pivot_longer(cols = -c(1:3), names_to = "Category", values_to = "scores") %>%
  group_by(date, state) %>%
  summarize(
    population = population,
    total_sum = sum(scores, na.rm = T)
  ) %>%
  ungroup() %>%
  unique() %>%
  mutate(weighted_score = (population * total_sum) / sum(unique(population))) %>%
  identity()

# standardized
weighted_std_scores <- dummies_with_pop %>%
  select(-Emergency_Declaration) %>% # is only "yes", so no sd, and would be centered on 0 anyway.
  pivot_longer(cols = -c(1:3), names_to = "Category", values_to = "scores") %>%
  group_by(Category) %>%
  summarize(
    state = state,
    date = as.Date(date),
    population = population,
    scores = scores,
    standardized_score = (scores - mean(scores, na.rm = T)) / sd(scores, na.rm = T),
    weighted_standardized_score = (population * standardized_score) / sum(unique(population), na.rm = T)
  ) %>%
  ungroup() %>%

  # save the panel version.
  write_csv(here("Intermediate_Data/weighted_std_scores_panel.csv")) %>% # This causes the date to change to chr...
  mutate(date = as.Date(date)) %>%

  # continue on to aggregate the states into nation-wide.
  select(date, weighted_standardized_score) %>%
  group_by(date) %>%
  summarize(composite_weighted_std_score = sum(weighted_standardized_score, na.rm = T)) %>%
  ungroup() %>%

  # Save the succinct index version
  write_csv(here("Results/csv/weighted_std_scores_agg.csv")) %>% # again changes date to chr type.
  mutate(date = as.Date(date)) %>%
  identity()

# ---- Weighted Plots ---------------------------------------------------------#
# 1: by state (facet wrapped)
weighted_scores %>%
  ggplot(aes(x = date, y = weighted_score)) +
  geom_line() +
  facet_wrap(~state, scales = "free_y") +
  theme_minimal() +
  theme(
    axis.text.y = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    axis.title.x = element_blank(),
    aspect.ratio = .5
  ) +
  scale_y_continuous(position = "right") +
  ggtitle(
    label = "Population-weighted scores of restriction severity",
    subtitle = "Low score means less severe restrictions."
  ) +
  labs(caption = paste0("Series is computed as (State Population * sum of lockdown categorical variables) / US Population).\n Data from ", min(weighted_std_scores$date), " through ", max(weighted_std_scores$date), ".")) +
  ggsave(here("Results/plots/pop_weighted_facet.png"), width = 12, height = 6)

# 2: US total
weighted_scores %>%

  # aggregate into US-wide scores.
  group_by(date) %>%
  summarise(weighted_score = sum(weighted_score)) %>%
  ungroup() %>%

  # continue with plotting
  ggplot(aes(x = date, y = weighted_score)) +
  geom_line(size = 1.25) +
  theme_minimal() +
  theme(
    panel.grid.major.x = element_line(size = 1, colour = "lightgrey"),
    panel.grid.minor.y = element_blank(),
    axis.title.x = element_blank(),
    aspect.ratio = .5
  ) +
  scale_y_continuous(position = "right") +
  scale_x_date(
    date_minor_breaks = "1 week",
    date_breaks = "1 month",
    date_labels = " %b \n %Y"
  ) +
  ggtitle(
    label = "US Population-Weighted Rescriction Severity Index",
    subtitle = "Low score means less severe restriction"
  ) +
  labs(caption = paste0("Series is computed as (State Population * sum of lockdown categorical variables) / US Population). \n Data is from ", min(weighted_std_scores$date), " through ", max(weighted_std_scores$date), ".")) +
  ylab("Severity Score (unitless)") +
  ggsave(here("Results/plots/pop_weighted_aggregated.png"), width = 12, height = 6)


# 3: Standardized score: THE HOLY GRAIL OF THESE CHARTS!!!!!!
weighted_std_scores %>%
  ggplot(aes(x = date, y = composite_weighted_std_score)) +
  geom_line(size = 1.25) +
  theme_minimal() +
  theme(
    panel.grid.major.x = element_line(size = 1, colour = "lightgrey"),
    panel.grid.minor.y = element_blank(),
    axis.title.x = element_blank(),
    aspect.ratio = .5
  ) +
  scale_y_continuous(position = "right") +
  scale_x_date(
    date_minor_breaks = "1 week",
    date_breaks = "1 month",
    date_labels = " %b \n %Y"
  ) +
  ggtitle(
    label = "US Populatiuon-Weighted Rescriction Severity Index (Standardized)",
    subtitle = "Low score means less severe restrictions."
  ) +
  labs(caption = paste0("Standardized severity is calculated by standardizing (subtract mean and divid by standard deviation) the nationally-aggregated scores across 9 categorical variables. \n Standardization is necessary to correctly weight each category, regardless of the number of levels of it. \n Data is from ", min(weighted_std_scores$date), " through ", max(weighted_std_scores$date), ".")) +
  ylab("Severity Index Score") +
  ggsave(here("Results/plots/pop_weighted_standardized_aggregate.png"), width = 12, height = 6)
