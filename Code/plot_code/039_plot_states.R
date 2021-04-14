# 039_plot_states
# plot facet wraps, and then map over each state

# ---- SETUP ------------------------------------------------------------------#
library(here)
library(tidyverse)
library(scales)
library(gghighlight)

weighted_std_scores <- read_csv(here("Results/csv/states_categories.csv"))

# ---- PLOTS ------------------------------------------------------------------#

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
    axis.title.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    aspect.ratio = .5
  ) +
  scale_y_continuous(position = "right") +
  ggtitle(
    label = "Restriction severity within each state mostly subsided after Spring, but some increased into Winter.",
    subtitle = "Y-axes vary to show how each state has changed over time."
  ) +
  labs(caption = paste0("Data: KFF State COVID-19 Data and Policy Actions\n",
                        "Data from ", min(weighted_std_scores$date), " through ", max(weighted_std_scores$date), ".\n",
                        "Calculations; Chart: Michael Boerman github.com/michaelboerman")) +
  ggsave(here("Results/plots/states_index_pop_std_free.png"), width = 16, height = 20)

# plot all on one, with highlights
weighted_std_scores %>%
  
  group_by(state, date) %>%
  mutate(weighted_standardized_score = sum(weighted_standardized_score, na.rm = TRUE)) %>%
  ungroup() %>%
  
  mutate(weighted_standardized_score = rescale(weighted_standardized_score, c(0,100))) %>% 
  
  ggplot(aes(x = date, y = weighted_standardized_score, color = state)) +
  geom_line(size = 1.25) +
  theme_minimal() +
  theme(
    panel.grid.major.x = element_line(size = 1, colour = "lightgrey"),
    panel.grid.minor.y = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    aspect.ratio = .5
  ) +
  scale_y_continuous(position = "right") +
  scale_x_date(
    date_minor_breaks = "1 week",
    date_breaks = "1 month",
    date_labels = " %b \n %Y"
  ) +
  gghighlight(state %in% c("CA", "TX", "FL"), label_key = state) +
  ggtitle(
    label = "Most states enacted similar policy strength. A few stand exceptionally high or low.",
    subtitle = "All axes are the same 0 to 100 scale of severity."
  ) +
  labs(caption = paste0("Standardized severity is calculated by a weighted sum of individual categories.\n",
                        "Data: KFF State COVID-19 Data and Policy Actions\n",
                        "Calculations; Chart: Michael Boerman github.com/michaelboerman.")) +
  ggsave(here(paste0("Results/plots/state_index/pop_std_all_states.png")), width = 12, height = 6)


# plot one state decomposition by facet wrap
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
    scale_x_date(
      date_minor_breaks = "1 week",
      date_breaks = "1 month",
      date_labels = "%b"
    ) +
    ggtitle(
      label = paste0(state_in, " Restrictions by Category"),
      subtitle = "Low score means less severe restrictions."
    ) +
    labs(caption = paste0("Fixed Y axis. Gaps in data are NAs.\n Data is from ", min(weighted_std_scores$date), " through ", max(weighted_std_scores$date), ".\n Data: Kaiser Family Foundation.\n Chart: Michael Boerman, github.com/michaelboerman")) +
    ggsave(here(paste0("Results/plots/state_categories/categories_", state_in, ".png")), width = 12, height = 6)
)

# plot each state decomp by bar chart / colors
map(unique(weighted_std_scores$state), function(state_in)
  weighted_std_scores %>%
    group_by(state, date, Category) %>%
    summarize(
      weighted_standardized_score = sum(weighted_standardized_score, na.rm = TRUE)
    ) %>% 
    filter(state == state_in) %>% 
    ggplot(aes(x = date)) +
    geom_bar(aes(y = weighted_standardized_score, fill = Category), 
             position = "stack", stat = "identity", alpha = 0.75, width = 1.0) +
    geom_hline(yintercept = 0, size = 1.0, color = "black") +
    theme_minimal() +
    theme(
      panel.grid.major.x = element_line(size = 1, colour = "lightgrey"),
      panel.grid.minor.y = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      aspect.ratio = .5
    ) +
    scale_y_continuous(position = "right") +
    scale_color_brewer(palette = "RdYlBu") +
    scale_x_date(
      date_minor_breaks = "1 week",
      date_breaks = "1 month",
      date_labels = "%b"
    ) +
    ggtitle(
      label = paste0(state_in, " Severity Decomposition by Category"),
    ) +
    labs(caption = paste0("Data: KFF State COVID-19 Data and Policy Actions\n",
                          "Calculations; Chart: Michael Boerman github.com/michaelboerman.")) +
    ggsave(here(paste0("Results/plots/state_decomp/decomp_", state_in, ".png")), width = 12, height = 6)
)

# plot the one index for each state
map(unique(weighted_std_scores$state), function(state_in)
  weighted_std_scores %>%
    
    group_by(state, date) %>%
    mutate(weighted_standardized_score = sum(weighted_standardized_score, na.rm = TRUE)) %>%
    ungroup() %>%
    
    mutate(weighted_standardized_score = rescale(weighted_standardized_score, c(0,100))) %>% 
    filter(state == state_in) %>% 
    
    ggplot(aes(x = date, y = weighted_standardized_score)) +
    geom_line(size = 1.25) +
    theme_minimal() +
    theme(
      panel.grid.major.x = element_line(size = 1, colour = "lightgrey"),
      panel.grid.minor.y = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
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
    ggsave(here(paste0("Results/plots/state_index/pop_std_", state_in, ".png")), width = 12, height = 6)
)

