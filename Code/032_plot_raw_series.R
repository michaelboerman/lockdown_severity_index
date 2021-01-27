# 032_plot_raw_series.R
# Michael Boerman January 2021
# 
# output: categorical_facet.png 
#         categorical_aggregate.png

# ---- Setup ------------------------------------------------------------------#

library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(here)

# un-comment and source the previous file if you are not going through sequentially.
# source(here("Code/021_make_factor_levels.R"))

# ---- Load Previous Data -----------------------------------------------------#
# From 021_make_factor_levels:

summary_df <- read_csv(here("Intermediate_Data/summary_df.csv"))

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
  labs(caption = paste0("Severity is calculated as the sum of 9 categorical variables mapped to numbers (dummy variables). NAs are counted as 0. \n Data is from ", min(summary_df$Date), " through ", max(summary_df$Date), ".")) +
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
  labs(caption = paste0("Severity is calculated as the sum of 9 categorical variables mapped to numbers (dummy variables). NAs are counted as 0. \n Data is from ", min(summary_df$Date), " through ", max(summary_df$Date), ".")) +
  ylab("Severity Index Score") +
  ggsave(here("Results/plots/categorical_aggregate.png"), width = 12, height = 6)
