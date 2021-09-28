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

# ---- Read in population and index data from 024
pop_weighted_scores <- read_csv(here("Intermediate_Data/dummies_with_pop.csv"),
  col_types = cols(
    .default = col_double(),      # coerce to be factors instead of chr
    date = col_date(format = ""), # except column Date, read as date.
    state = col_character()       # except column Locaiton, read as chr
  )
)

# ---- Weight by population ---------------------------------------------------#
# Next, we'll group by the date and the state to come up with sum of dummies.
# Basically want one score instead of the 9 individual components.

# weighted but not standardized
pop_weighted_scores <- pop_weighted_scores %>%
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
  labs(caption = paste0("Series is computed as (State Population * sum of lockdown categorical variables) / US Population).\n Data from ", min(pop_weighted_scores$date), " through ", max(pop_weighted_scores$date), "."))
  ggsave(here("Results/plots/unused_intermediates/states_facet_pop.png"), width = 12, height = 6)

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
  ylab("Severity Score (unitless)")
  ggsave(here("Results/plots/unused_intermediates/national_index_pop.png"), width = 12, height = 6)
