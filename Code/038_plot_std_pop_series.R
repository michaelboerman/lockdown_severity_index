# 038_plot_std_pop_series.R
# Michael Boerman January 2021

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

dummies_with_pop    <- read_csv(here("Intermediate_Data/dummies_with_pop.csv"), 
  col_types = cols(
    .default = col_double(),      # coerce to be factors instead of chr
    date = col_date(format = ""), # except column Date, read as date.
    state = col_character()       # except column state, read as chr
  )
)

pop_weighted_scores <- read_csv(here("Intermediate_Data/pop_weighted_scores.csv"))


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
