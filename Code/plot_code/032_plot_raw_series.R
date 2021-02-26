# 032_plot_raw_series.R
# Michael Boerman January 2021
# 
# output: categories_states.png 
#         categories_national.png

# ---- Setup ------------------------------------------------------------------#

library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(here)

# ---- Load Previous Data -----------------------------------------------------#
# From 021_make_factor_levels:

summary_df <- read_csv(here("Intermediate_Data/summary_df.csv"))

# ---- Plots ------------------------------------------------------------------#

# 1: All Categories (facet wrapped)
summary_df %>%
  ggplot(aes(x = Date, y = sums)) +
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
  scale_x_date(date_breaks = "2 month", date_labels = "%b",
               date_minor_breaks = "1 month") +
  ggtitle(
    label = "United States Restrictions by Category",
    subtitle = "Low score means less severe restrictions. Y axis fixed."
  ) +
  labs(caption = paste0("NAs are counted as 0.\n Data is from ", min(summary_df$Date), " through ", max(summary_df$Date), ".\n Data: Kaiser Family Foundation.\n Chart: Michael Boerman, github.com/michaelboerman")) +
  ggsave(here("Results/plots/category_decomp.png"), width = 12, height = 6)

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
    label = "US Rescriction Severity: Un-standardized and un-weighted",
    subtitle = "Low score means less severe restrictions."
  ) +
  labs(caption = paste0("Severity is calculated as the unweighted sum of 9 categorical variables assigned to numbers. NAs are counted as 0.\n Data is from ", min(summary_df$Date), " through ", max(summary_df$Date), ".")) +
  ylab("Severity") +
  ggsave(here("Results/plots/unused_intermediates/national_index_plain.png"), width = 12, height = 6)
