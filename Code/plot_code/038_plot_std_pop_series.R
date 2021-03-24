# 038_plot_std_pop_series.R
# Michael Boerman January 2021


# ---- Setup ------------------------------------------------------------------#

library(readr)
library(dplyr)
library(tidyr)
library(purrr)
library(ggplot2)
library(zoo)
library(here)
library(scales)

options(scipen = 999) # for the population-weighted scores

# ---- Load Previous Data -----------------------------------------------------#
# From 024:

dummies_with_pop <- 
  read_csv(here("Intermediate_Data/dummies_with_pop.csv"), 
    col_types = cols(
      .default = col_double(),      # defualt is double
      date = col_date(format = ""), # except column Date, read as date.
      state = col_character()       # except column state, read as chr
  )
)

# standardize and weight by pop
weighted_std_scores <- dummies_with_pop %>%
  select(-Emergency_Declaration) %>% # is only "yes", so no sd, and would be centered on 0 anyway.
  pivot_longer(cols = -c(1:3), names_to = "Category", values_to = "scores") %>%
  mutate(scores_rescaled = rescale(scores, c(0,100))) %>% 
  group_by(Category) %>%
  summarize(
    state = state,
    date = as.Date(date),
    population = population,
    scores = scores_rescaled,
    standardized_score = (scores_rescaled - mean(scores_rescaled, na.rm = T)) / sd(scores_rescaled, na.rm = T),
    weighted_standardized_score = (population * standardized_score) / sum(unique(population), na.rm = T),
  ) %>% 
  ungroup() %>% 
  identity()

write_csv(weighted_std_scores, here("Results/csv/pop_std_scores_states.csv"))

# plot by state: fixed y axis
weighted_std_scores %>%
  
  group_by(date, state) %>% 
  mutate(weighted_standardized_score = sum(weighted_standardized_score, na.rm = TRUE)) %>% 
  ungroup() %>% 
  
  mutate(weighted_standardized_score = rescale(weighted_standardized_score, c(0,100))) %>% 
  ggplot(aes(x = date, y = weighted_standardized_score)) +
  geom_line() +
  facet_wrap(~state, scales = "fixed", nrow = 10) +
  theme_minimal() +
  theme(
    text = element_text(size = 20),
    axis.text.y = element_blank(),
    axis.text.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    axis.title.x = element_blank(),
    aspect.ratio = .5
  ) +
  scale_y_continuous(position = "right") +
  ggtitle(
    label = "Population-weighted, Standardized Scores of Restriction Severity",
    subtitle = "All same y axis. For plots of each state, see Results/plots/state_index/"
  ) +
  labs(caption = paste0("Data: KFF State COVID-19 Data and Policy Actions\n",
                        "Data from ", min(weighted_std_scores$date), " through ", max(weighted_std_scores$date), ".\n",
                        "Calculations; Chart: Michael Boerman github.com/michaelboerman")) +
  ggsave(here("Results/plots/states_index_pop_std_fixed.png"), width = 12, height = 20)


# plot by state: free y axis
weighted_std_scores %>%
  
  group_by(date, state) %>% 
  mutate(weighted_standardized_score = sum(weighted_standardized_score, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(weighted_standardized_score = rescale(weighted_standardized_score, c(0,100))) %>% 

  ggplot(aes(x = date, y = weighted_standardized_score)) +
  geom_line() +
  facet_wrap(~state, scales = "free", nrow = 10) +
  theme_minimal() +
  theme(
    text = element_text(size = 20),
    axis.text.y = element_blank(),
    axis.text.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    axis.title.x = element_blank(),
    aspect.ratio = .5
  ) +
  scale_y_continuous(position = "right") +
  ggtitle(
    label = "Population-weighted, Standardized Scores of Restriction Severity",
    subtitle = "Y scales vary. For plots of each state, see Results/plots/state_index/"
  ) +
  labs(caption = paste0("Data: KFF State COVID-19 Data and Policy Actions\n",
                        "Data from ", min(weighted_std_scores$date), " through ", max(weighted_std_scores$date), ".\n",
                        "Calculations; Chart: Michael Boerman github.com/michaelboerman")) +
  ggsave(here("Results/plots/states_index_pop_std_free.png"), width = 16, height = 20)

# continue on to aggregate the states into nation-wide.
weighted_std_scores_agg <- 
  weighted_std_scores %>% 
  select(date, weighted_standardized_score) %>%
  group_by(date) %>%
  summarize(composite_weighted_std_score = sum(weighted_standardized_score, na.rm = T)) %>%
  ungroup() %>%
  mutate(composite_weighted_std_score = rescale(composite_weighted_std_score, c(0,100))) %>%

  # Save the succinct index version
  write_csv(here("Results/csv/national_index_pop_std.csv")) %>% # again changes date to chr type.
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
    subtitle = "Low score means less severe restrictions. Mapped to [0, 100]."
  ) +
  labs(caption = paste0("Standardized severity is calculated by a weighted sum of individual categories, then weighted by state by population.\n",
                        "Data: KFF State COVID-19 Data and Policy Actions\n",
                        "Calculations; Chart: Michael Boerman github.com/michaelboerman.")) +
  ylab("Severity Index Score") +
  ggsave(here("Results/plots/national_index_pop_std.png"), width = 12, height = 6)

# Plot Decomposition bar chart
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
    label = "Contribution to Index by Category",
    subtitle = "Some categories play a big role early and become less important over time."
  ) +
  labs(caption = paste0("Lockdown severity is standardized within each category, which are then weighted according to state population.\n",
                        "Data: KFF State COVID-19 Data and Policy Actions\n",
                        "Calculations; Chart: Michael Boerman github.com/michaelboerman.")) +
  ylab("Contribution Amount") +
  ggsave(here("Results/plots/unused_intermediates/national_decomp_bar_pop_std.png"), width = 16, height = 20)

#plot decomp facet wrapped
weighted_std_scores %>%
  
  # mutate(weighted_standardized_score = rescale(weighted_standardized_score, c(0,100))) %>%
  group_by(date, Category) %>%
  summarize(
    weighted_standardized_score = sum(weighted_standardized_score, na.rm = TRUE)
  ) %>% 
  # mutate(weighted_standardized_score = rescale(weighted_standardized_score, c(0,100))) %>%
  
  ggplot(aes(x = date, y = weighted_standardized_score)) +
  geom_line() +
  facet_wrap(~Category, scales = "fixed") + # fix to show the impact of each if not standardized.
  theme_minimal() +
  theme(
    axis.title.y = element_blank(),
    axis.title.x = element_blank(),
    # axis.text.y = element_blank(),
    legend.position = "none",
    aspect.ratio = .5
  ) +
  scale_x_date(date_breaks = "2 month", date_labels = "%b",
               date_minor_breaks = "1 month") +
  ggtitle(
    label = "United States Restrictions by Category",
    subtitle = "Y axis fixed. Low score means less severe restrictions."
  ) +
  labs(caption = paste0("NAs are counted as 0.\n Data is from ", min(weighted_std_scores$date), " through ", max(weighted_std_scores$date), ".\n Data: Kaiser Family Foundation.\n Chart: Michael Boerman, github.com/michaelboerman")) +
  ggsave(here("Results/plots/national_decomp_facet_pop_std.png"), width = 16, height = 20)


