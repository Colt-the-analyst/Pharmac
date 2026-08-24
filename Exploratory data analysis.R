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

# 15 most common medicines by number of distributions
Common_chem_15 <- Data_ByChemical_clean |>
  mutate(
    NumDisps = as.numeric(
      case_when(
        NumDisps == "<6" ~ "2.5", 
        .default = NumDisps)
      )
    ) |>
  group_by(Chemical) |>
  summarise(NumDisps = sum(NumDisps)) |>
  ungroup() |>
  slice_max(NumDisps, n = 15) |>
  pull(Chemical)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# Suppression
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

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

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# Chemical dispension count
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

Data_ByChemical_count <- Data_ByChemical_clean |>
  # Impute the suppressed values with the median over the range (2.5)
  mutate(NumDisps = as.numeric(
    case_when(
      NumDisps == "<6" ~ "2.5",
      .default = NumDisps
    )
  )) |>
  group_by(Chemical) |>
  summarise(NumDisps = sum(NumDisps)) |>
  ungroup() |>
  arrange(desc(NumDisps))

Data_ByChemical_year_count <- Data_ByChemical_clean |>
  # Impute the suppressed values with the median over the range (2.5)
  mutate(NumDisps = as.numeric(
    case_when(
      NumDisps == "<6" ~ "2.5",
      .default = NumDisps
    )
  )) |>
  group_by(YearDisp, Chemical) |>
  summarise(NumDisps = sum(NumDisps)) |>
  ungroup() |>
  arrange(YearDisp, desc(NumDisps)) |>
  group_by(YearDisp) |>
  mutate(Rank = row_number()) |>
  slice_min(Rank, n = 10) |>
  ungroup()

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# Concentrated Medication by District
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

# Find the regions where each medication has the highest concentration of
# distribution
Chemical_concentration_region <- Data_ByChemical_clean |>
  filter(District != "Unknown") |>
  mutate(
    NumDisps = as.numeric(case_when(
      NumDisps == "<6" ~ "2.5",
      .default = NumDisps
    )
  )) |>
  group_by(Chemical, District) |>
  summarise(NumDisps = sum(NumDisps)) |>
  ungroup() |>
  group_by(Chemical) |>
  mutate(
    Chemical_total = sum(NumDisps),
    district_share = NumDisps / Chemical_total,
    districts_observed = n()
  ) |>
  slice_max(
    district_share, n = 1
  ) |>
  ungroup() |>
  arrange(desc(district_share))

# ------------------------------------------------------------------------------
# Modelling
# ------------------------------------------------------------------------------

# Run a chi squared test to see if the chemicals and districts are independent,
# meaning that the distribution across chemicals is the same for each district
chi_data <- Data_ByChemical_clean |>
  filter(
    Chemical %in% Common_chem_15,
    District != "Unknown"
  ) |>
  mutate(
    NumDisps = case_when(
      NumDisps == "<6" ~ 2.5,
      TRUE ~ as.numeric(NumDisps)
    )
  ) |>
  group_by(District, Chemical) |>
  summarise(
    NumDisps = sum(NumDisps)
  ) |>
  ungroup()

dispensing_table <- xtabs(
  NumDisps ~ District + Chemical,
  data = chi_data
)

(chi_result <- chisq.test(dispensing_table))

# Pearson's Chi-squared test
# 
# data:  dispensing_table
# X-squared = 5642009, df = 266, p-value < 2.2e-16

# These results provide strong evidence that dispensing patterns are different
# between districts. The p-value is much smaller than 0.05, so we reject the
# null hypothesis that district and chemical dispensing are independent.

# ------------------------------------------------------------------------------
# Graphics
# ------------------------------------------------------------------------------


# Histogram of the most dispersed drugs
Data_ByChemical_count |>
  slice_max(NumDisps, n = 25) |>
  mutate(Chemical = fct_reorder(Chemical, NumDisps)) |>
  ggplot(aes(x = NumDisps, y = Chemical)) +
  geom_col() +
  scale_x_continuous() +
  labs(
    title = "Histogram of the most dispersed drugs",
    x = "Number of distributions",
    y = "Chemical",
    colour = NULL
  )

# Rankings of the most frequently dispensed medications over time
Data_ByChemical_year_count |>
  ggplot(aes(
    x = YearDisp,
    y = Rank,
    colour = Chemical,
    group = Chemical
  )) +
  geom_line(
    linewidth = 1.2,
    alpha = 0.8
  ) +
  geom_point(size = 3) +
  scale_y_reverse(
    breaks = 1:10
  ) +
  labs(
    title = "Rankings of the most frequently dispensed medications over time",
    x = "Year",
    y = "Rank"
  )

# Dispensing patterns across dristricts
# Note that this hasn't been adjusted for population size in each district
chi_data |>
  ggplot(
    aes(
      x = Chemical, y = District, fill = log1p(NumDisps)
    )
  ) +
  geom_tile() +
  scale_fill_viridis_c() +
  labs(title = "Dispensing patterns across dristricts") +
  theme(
    axis.text.x = element_text(
      angle = 45,
      hjust = 1
    )
  )

# Districts with the highest dispensing share
Chemical_concentration_region |>
  count(
    District,
    name = "chemical_count"
  ) |>
  ggplot(aes(
    x = chemical_count,
    y = fct_reorder(District, chemical_count)
  )) +
  geom_col() +
  geom_text(
    aes(label = chemical_count),
    hjust = -0.2
  ) +
  labs(
    title = "Districts with the highest dispensing share",
    x = "Number of chemicals",
    y = NULL
  )

# Distribution of the largest district dispensing shares
Chemical_concentration_region |>
  ggplot(aes(x = district_share)) +
  geom_histogram(
    bins = 30,
    colour = "white"
  ) +
  geom_vline(
    xintercept = median(
      Chemical_concentration_region$district_share
    ),
    linetype = "dashed"
  ) +
  annotate(
    "text",
    x = median(
      Chemical_concentration_region$district_share
    ),
    y = Inf,
    label = paste0("Median = ", round(
      median(
        Chemical_concentration_region$district_share
      ),
      digits = 4
    )),
    hjust = -0.1,
    vjust = 1.5
  ) +
  labs(
    title = "Distribution of the largest district dispensing shares",
    x = "Largest district share of dispensings",
    y = "Number of chemicals"
  )
