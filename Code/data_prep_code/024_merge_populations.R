# 024 merge with population data

# read in from 021_make_factor_levels output.
cat_data_reordered <- read_csv(here("Intermediate_Data/cat_data_reordered.csv"),
  col_types = cols(
    .default = col_double(), # coerce to be factors instead of chr
    Date = col_date(format = ""), # except column Date, read as date.
    Location = col_character() # except column Locaiton, read as chr
  )
)

state_populations <- read_csv(here("Intermediate_Data/state_populations.csv"))

# ---- Merge and Clean --------------------------------------------------------#

# Goal: need to change from full state names to be the abbreviations.
#   however, state.name and .abb don't contain DC.

# https://stackoverflow.com/questions/5411979/state-name-to-abbreviation solution #2 in first comment (vectorized)

# Note: The state populations appear static. thus, the national population is static, too.

# First, we create a dataframe that has the state populations included with the dummies.
dummies_with_pop <- cat_data_reordered %>%
  mutate(
    Location = as.character(Location),
  ) %>%
  mutate(
    state = case_when(
      Location %in% state.name ~ setNames(state.abb, state.name)[Location],
      Location == "District of Columbia" ~ "DC",
      TRUE ~ "Error!"
    ),
    .after = Date
  ) %>%

  # this is sketchy. repeating the state_pop instead of matchig it over. merge wasn't working.
  mutate(
    population = rep(state_populations$population, times = nrow(cat_data_reordered) / 51),
    .after = state
  ) %>%
  rename(date = Date) %>%
  select(-Location) %>%
  identity()

# piping right into a write_csv messes up the date column. Thus, I terminate.
write_csv(dummies_with_pop, here("Intermediate_Data", "dummies_with_pop.csv"))
