#run_all.R

# to execute all scripts in order. Do this to ensure all intermediate data exists.

library(here)

# data prep scripts
source(here("code/data_prep_code", "010_compile_kff_data.R"))
source(here("code/data_prep_code", "012_gather_populations.R"))
source(here("code/data_prep_code", "021_make_factor_levels.R"))

# plotting scripts
source(here("code/plot_code", "032_plot_raw_series.R"))
source(here("code/plot_code", "035_plot_std_series.R"))
source(here("code/plot_code", "037_plot_pop_series.R"))
source(here("code/plot_code", "038_plot_std_pop_series.R"))
