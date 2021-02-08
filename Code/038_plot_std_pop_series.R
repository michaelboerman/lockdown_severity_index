# 038_plot_std_pop_series.R
# Michael Boerman January 2021

# output: pop_std_scores_states.csv
#         pop_std_states.png
#         pop_std_scores_national.csv
#         pop_std_national.png

# ---- Setup ------------------------------------------------------------------#

library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(zoo)
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

# standardize and weight by pop
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
  ungroup()

write_csv(weighted_std_scores, here("Results/csv/pop_std_scores_states.csv"))

# plot by state
weighted_std_scores %>%
  group_by(date, state) %>% 
  mutate(weighted_standardized_score = sum(weighted_standardized_score, na.rm = TRUE)) %>% 
  ggplot(aes(x = date, y = weighted_standardized_score)) +
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
    label = "Population-weighted, standardized scores of restriction severity",
    subtitle = "Low score means less severe restrictions."
  ) +
  labs(caption = paste0("Data: KFF State COVID-19 Data and Policy Actions\n",
                        "Data from ", min(weighted_std_scores$date), " through ", max(weighted_std_scores$date), ".\n",
                        "Calculations; Chart: Michael Boerman github.com/michaelboerman")) +
  ggsave(here("Results/plots/pop_std_states.png"), width = 12, height = 6)


# continue on to aggregate the states into nation-wide.
weighted_std_scores_agg <- 
  weighted_std_scores %>% 
  
  select(date, weighted_standardized_score) %>%
  group_by(date) %>%
  summarize(composite_weighted_std_score = sum(weighted_standardized_score, na.rm = T)) %>%
  ungroup() %>%
  
  # Save the succinct index version
  write_csv(here("Results/csv/pop_std_scores_national.csv")) %>% # again changes date to chr type.
  mutate(date = as.Date(date)) %>%
  identity()


# 3: Standardized score: THE HOLY GRAIL OF THESE CHARTS!!!!!!
weighted_std_scores_agg %>%
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
    label = "U.S. Restriction Severity Index",
    subtitle = "Low score means less severe restrictions."
  ) +
  labs(caption = paste0("Standardized severity is calculated by weighting the standardized severity index of each state by population.\n",
                        "Data: KFF State COVID-19 Data and Policy Actions\n",
                        "Calculations; Chart: Michael Boerman github.com/michaelboerman.")) +
  ylab("Severity Index Score") +
  ggsave(here("Results/plots/pop_std_national.png"), width = 12, height = 6)


# plot states as light shades all on one chart
# weighted_std_scores %>%
#   group_by(date, state) %>% 
#   summarize(
#     weighted_standardized_score = sum(weighted_standardized_score, na.rm = TRUE),
#     pop_weighted_avg = mean(weighted_standardized_score)
#     ) %>% 
#   ggplot(aes(x = date)) +
#   geom_line(aes(y = weighted_standardized_score, color = state)) +
#   geom_line(aes(y = pop_weighted_avg), color = "blue")
#   NULL

# Plot Decomposition
weighted_std_scores %>%
  group_by(date, Category) %>%
  summarize(
    weighted_standardized_score = sum(weighted_standardized_score, na.rm = TRUE)
  ) %>% 
  ggplot(aes(x = date)) +
  geom_bar(aes(y = weighted_standardized_score, fill = Category), 
           position = "stack", stat = "identity", alpha = 0.75) +
  geom_hline(yintercept = 0, size = 1.0, color = "black") +
  theme_minimal() +
  theme(
    panel.grid.major.x = element_line(size = 1, colour = "lightgrey"),
    panel.grid.minor.y = element_blank(),
    axis.title.x = element_blank(),
    aspect.ratio = .5
  ) +
  scale_y_continuous(position = "right") +
  scale_color_brewer(palette = "RdYlBu") +
  scale_x_date(
    date_minor_breaks = "1 week",
    date_breaks = "1 month",
    date_labels = " %b \n %Y"
  ) +
  ggtitle(
    label = "Severity Decomposition by Category",
    subtitle = " Some categories play a big role early and become less important over time."
  ) +
  labs(caption = paste0("Lockdown severity is standardized within each category, which are then weighted according to state population.\n",
                        "Data: KFF State COVID-19 Data and Policy Actions\n",
                        "Calculations; Chart: Michael Boerman github.com/michaelboerman.")) +
  ylab("Severity Index Value") +
  ggsave(here("Results/plots/pop_std_categories_decomp.png"), width = 12, height = 6)
