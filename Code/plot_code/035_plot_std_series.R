# 035_plot_std_series.R
# Michael Boerman January 2021

# output: standardized_categories_national.png 


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

# ---- STANDARDIZE THE SCORES -------------------------------------------------#

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

# Plot
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
    label = "US Rescriction Severity: Standardized each Category",
    subtitle = "Low score means less severe restrictions."
  ) +
  labs(caption = paste0("Standardized severity is calculated by subtract mean and divid by standard deviation across 9 categorical variables.\n Standardization is necessary to correctly weight each category, regardless of its number of levels.\n Data is from ", min(standardized_scores$Date), " through ", max(standardized_scores$Date), ".")) +
  ylab("Severity Index Score")
  ggsave(here("Results/plots/unused_intermediates/national_index_std.png"), width = 12, height = 6)
