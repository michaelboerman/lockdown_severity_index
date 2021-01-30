# 037_plot_pop_std_series.R
# Michael Boerman January 2021
#
# weight by state population
#
# output: dummies_with_pop.csv
#         pop_weighted_scores_states.csv
#         pop_weighted_states.png
#         pop_weighted_national.png


# ---- Setup ------------------------------------------------------------------#

library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(here)

options(scipen = 999) # for the population-weighted scores

# un-comment and source the previous file if you are not going through sequentially.
# source(here("Code/021_make_factor_levels.R"))
# source(here("Code/012_gather_populations.R"))

# ---- Load Previous Data -----------------------------------------------------#
# From 021_make_factor_levels:

cat_data_reordered <- read_csv(here("Intermediate_Data/cat_data_reordered.csv"),
  col_types = cols(
    .default = col_double(),      # coerce to be factors instead of chr
    Date = col_date(format = ""), # except column Date, read as date.
    Location = col_character()    # except column Locaiton, read as chr
  )
)

state_populations <- read_csv(here("Intermediate_Data/state_populations.csv"))

# ---- Merge and Clean --------------------------------------------------------#

# Goal: need to change from full state names to be the abbreviations.
#   however, state.name and .abb don't contain DC.

# https://stackoverflow.com/questions/5411979/state-name-to-abbreviation solution #2 in first comment (vectorized)

# Note: The state populations appear static. thus, the national population is static, too.

# First, we create a dataframe that has the state populations included with the dummies.
dummies_with_pop <- cat_data_reordered %>%
  mutate(
    Location = as.character(Location),
  ) %>%
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
    population = rep(state_populations$population, times = nrow(cat_data_reordered) / 51),
    .after = state
  ) %>%
  rename(date = Date) %>%
  select(-Location) %>%
  identity()

# piping right into a write_csv messes up the date column. Thus, I terminate.
write_csv(dummies_with_pop, here("Intermediate_Data", "dummies_with_pop.csv"))


# ---- Weight by population ---------------------------------------------------#
# Next, we'll group by the date and the state to come up with sum of dummies.
# Basically want one score instead of the 9 individual components.

# weighted but not standardized
pop_weighted_scores <- dummies_with_pop %>%
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

# Likewise. Terminate so as to preserve date format for this script.
write_csv(pop_weighted_scores, here("Intermediate_Data", "pop_weighted_scores_states.csv"))

# ---- Population-Weighted Plots ---------------------------------------------------------#

# 1: by state (facet wrapped)
pop_weighted_scores %>%
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
  labs(caption = paste0("Series is computed as (State Population * sum of lockdown categorical variables) / US Population).\n Data from ", min(pop_weighted_scores$date), " through ", max(pop_weighted_scores$date), ".")) +
  ggsave(here("Results/plots/pop_weighted_states.png"), width = 12, height = 6)

# 2: US total
pop_weighted_scores %>%

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
  labs(caption = paste0("Series is computed as (State Population * sum of lockdown categorical variables) / US Population). \n Data is from ", min(pop_weighted_scores$date), " through ", max(pop_weighted_scores$date), ".")) +
  ylab("Severity Score (unitless)") +
  ggsave(here("Results/plots/pop_weighted_national.png"), width = 12, height = 6)
