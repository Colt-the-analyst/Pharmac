# ==============================================================================
# Purpose:
#   Run a round of exploratory data analysis on the pharmaceutical data
# 
# Created:
#   23/08/2026
# ==============================================================================

# ------------------------------------------------------------------------------
# Setup
# ------------------------------------------------------------------------------

# Clear the environment
rm(list = ls())

# load the packages
pacman::p_load(
  tidyverse            # Data wrangling + graphics
)

# Set the seed
set.seed(111)

# Load the data
Data_ByChemical <- read.csv("Data/Data_ByChemical.csv") |>
  as_tibble()

# ------------------------------------------------------------------------------
# Initial tidying
# ------------------------------------------------------------------------------

# Remove the totals
Data_ByChemical_clean <- Data_ByChemical |>
  filter(District != "New Zealand")

# ------------------------------------------------------------------------------
# Summaries
# ------------------------------------------------------------------------------

# Check that the data is sufficiently suppressed
Data_ByChemical_suppressed_counts <- Data_ByChemical |> 
  mutate(
    suppressed_disp = NumDisps == "<6",
    suppressed_people = NumPpl == "<6"
    ) |>
  group_by(YearDisp, Chemical, Type) |>
  summarise(
    suppressed_people_count = sum(suppressed_people),
    suppressed_disp_count = sum(suppressed_disp)
    ) |>
  ungroup()

Data_ByChemical_suppression_problems <- Data_ByChemical_suppressed_counts |>
  filter(suppressed_disp_count == 1) |>
  arrange(suppressed_disp_count)

# An example of YearDisp, Chemical and Type combination with 
# insufficient suppression
Example_problem_values <- Data_ByChemical_suppression_problems |>
  head(1) |>
  select(YearDisp, Chemical, Type)

Data_ByChemical_suppression_problems_xmpl <- Data_ByChemical |>
  filter(
    YearDisp == pull(Example_problem_values, YearDisp),
    Chemical == pull(Example_problem_values, Chemical),
    Type == pull(Example_problem_values, Type)
  )

# South Canterbury has the NumDisps attribute supressed with the value of "<6"
# but the true value can be easily found with the following procedure
Data_ByChemical_suppression_problems_xmpl |>
  filter(District != "South Canterbury") |>
  mutate(
    NumDisps = as.numeric(NumDisps) + rbinom(n = n(), size = 1, prob = 0.5), # Add randomness so to not break any privacy rules
    Is_total = District == "New Zealand"
    ) |>
  group_by(Is_total) |>
  summarise(NumDisps = sum(NumDisps)) |>
  ungroup() |>
  pivot_wider(names_from = Is_total, values_from = NumDisps) |>
  mutate(
    # Account for the additions in NumDisps for each of the districts
    `TRUE` = `TRUE` + (nrow(Data_ByChemical_suppression_problems_xmpl) - 1) / 2, 
    Unsupressed_value = `TRUE` - `FALSE`
    ) |>
  pull(Unsupressed_value)

# Find the proportion of YearDisp, Chemical and Type combinations that have
# insufficient suppressions
Data_ByChemical_suppressed_counts |>
  mutate(one_suppressed = suppressed_people_count == 1 | suppressed_disp_count == 1) |>
  count(one_suppressed) |>
  pivot_wider(names_from = one_suppressed, values_from = n) |>
  mutate(proportion = `TRUE` / (`FALSE` + `TRUE`)) |>
  pull(proportion)
# [1] 0.1061375

# This means that roughly 11% of the combinations of all YearDisp, Chemical and
# Type have suppressions that are insufficient.

# The reason that the having instances of counts less than 6 is problematic is 
# that small values in these columns increase the risk of identifying someone
# from a rare medicine as well as district and year.

# ------------------------------------------------------------------------------
# Modelling
# ------------------------------------------------------------------------------

# ------------------------------------------------------------------------------
# Graphics
# ------------------------------------------------------------------------------

