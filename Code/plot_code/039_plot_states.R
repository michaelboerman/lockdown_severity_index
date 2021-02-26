# 039_plot_states

# map over each state
library(here)
library(tidyverse)
library(scales)

weighted_std_scores <- read_csv(here("Results/csv/pop_std_scores_states.csv"))

# plot one state decomposition
map(unique(weighted_std_scores$state), function(state_in)
  weighted_std_scores %>%
    filter(state == state_in) %>% 
    ggplot(aes(x = date, y = weighted_standardized_score, color = Category)) +
    geom_line() +
    facet_wrap(~Category, scales = "fixed") + # fix to show the impact of each if not standardized.
    theme_minimal() +
    theme(
      axis.title.y = element_blank(),
      axis.title.x = element_blank(),
      axis.text.y = element_blank(),
      legend.position = "none",
      aspect.ratio = .5
    ) +
    ggtitle(
      label = paste0(state_in, " Restrictions by Category"),
      subtitle = "Low score means less severe restrictions."
    ) +
    labs(caption = paste0("Fixed Y axis. Gaps in data are NAs.\n Data is from ", min(weighted_std_scores$Date), " through ", max(weighted_std_scores$Date), ".\n Data: Kaiser Family Foundation.\n Chart: Michael Boerman, github.com/michaelboerman")) +
    ggsave(here(paste0("Results/plots/state_categories/categories_", state_in, ".png")), width = 12, height = 6)
)

# plot the one final line for each state
map(unique(weighted_std_scores$state), function(state_in)
  weighted_std_scores %>%
    filter(state == state_in) %>% 
    group_by(date) %>% 
    mutate(weighted_standardized_score = sum(weighted_standardized_score, na.rm = TRUE)) %>% 
    ggplot(aes(x = date, y = weighted_standardized_score)) +
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
      label = paste0(state_in, " Restriction Severity Index"),
      subtitle = "Low score means less severe restrictions."
    ) +
    labs(caption = paste0("Standardized severity is calculated by a weighted sum of individual categories.\n",
                          "Data: KFF State COVID-19 Data and Policy Actions\n",
                          "Calculations; Chart: Michael Boerman github.com/michaelboerman.")) +
    ylab("Severity Index Score") +
    ggsave(here(paste0("Results/plots/state_index/pop_std_", state_in, ".png")), width = 12, height = 6)
)