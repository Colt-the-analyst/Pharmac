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
  tidyverse,           # Data wrangling + graphics
  tibble,              # Alternative to dataframes with slightly altered rules
  janitor,             # Tidy dataframe names
  scales               # Percentages in graphics
)

# Set the seed
set.seed(111)

# Load the data
Data_ByChemical <- read.csv("Data/Data_ByChemical.csv") |>
  as_tibble()
Deprivation <- read.csv("Data/Deprivation index 2023.csv") |>
  as_tibble() |>
  clean_names()
TG2_data <- read.csv("Data/Data_ByTG2.csv") |>
  as_tibble() |>
  clean_names()

# Define deprivation order
deprivation_order <- c(
  "1 - Least deprived",
  "2", "3", "4", "5", "6", "7", "8", "9",
  "10 - Most deprived",
  "Not elsewhere included",
  "Total stated - New Zealand index of socioeconomic deprivation",
  "Total - New Zealand index of socioeconomic deprivation"
)

# Approximate geographic order from north to south
district_order <- c(
  "Northland",
  "Waitematā",
  "Auckland",
  "Counties Manukau",
  "Waikato",
  "Bay of Plenty",
  "Lakes",
  "Tairāwhiti",
  "Taranaki",
  "Whanganui",
  "MidCentral",
  "Hawke's Bay",
  "Capital, Coast and Hutt Valley",
  "Wairarapa",
  "Nelson Marlborough",
  "West Coast",
  "Canterbury",
  "South Canterbury",
  "Southern",
  "Area outside health region"
)

# Create a vector just for dementia medication
dementia_chemicals <- c(
  "Donepezil hydrochloride",
  "Rivastigmine"
)

# ------------------------------------------------------------------------------
# Initial tidying
# ------------------------------------------------------------------------------

# Remove the totals
Data_ByChemical_clean <- Data_ByChemical |>
  filter(District != "New Zealand") |>
  # Rename the districts for merging purposes
  mutate(
    District = case_when(
      District == "Tairawhiti" ~ "Tairāwhiti",
      District == "Waitemata" ~ "Waitematā",
      District %in% c("Capital & Coast", "Hutt Valley") ~ "Capital, Coast and Hutt Valley",
      .default = District
    ),
    NumDisps = as.numeric(
      case_when(
        NumDisps == "<6" ~ "2.5", 
        .default = NumDisps)
    ),
    NumPpl = as.numeric(
      case_when(
        NumPpl == "<6" ~ "2.5", 
        .default = NumPpl)
    )
  ) |>
  # Summarise the Capital, Coast and Hutt Valley instances
  group_by(Type, ChemID, Chemical, District, YearDisp, NHIComp) |>
  summarise(
    NumDisps = sum(NumDisps),
    NumPpl = sum(NumPpl)
  )

# ------------------------------------------------------------------------------
# Summaries
# ------------------------------------------------------------------------------

# 15 most common medicines by number of distributions
Common_chem_15 <- Data_ByChemical_clean |>
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
  group_by(Chemical) |>
  summarise(NumDisps = sum(NumDisps)) |>
  ungroup() |>
  arrange(desc(NumDisps))

Data_ByChemical_year_count <- Data_ByChemical_clean |>
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

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# Deprivation pivot
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

# Change the raw data to wide format
Deprivation_wide <- Deprivation |>
  select(area, new_zealand_index_of_socioeconomic_deprivation, obs_value) |>
  pivot_wider(names_from = area, values_from = obs_value) |>
  mutate(
    new_zealand_index_of_socioeconomic_deprivation = factor(
      new_zealand_index_of_socioeconomic_deprivation,
      levels = deprivation_order,
      ordered = TRUE
    )
  ) |>
  arrange(new_zealand_index_of_socioeconomic_deprivation)

# Create a lookup of average deprivations per district
Deprivation_average <- Deprivation_wide |>
  mutate(
    index = parse_number(
      as.character(
        new_zealand_index_of_socioeconomic_deprivation
      )
    )
  ) |>
  filter(index %in% 1:10) |>
  pivot_longer(
    cols = -c(
      new_zealand_index_of_socioeconomic_deprivation,
      index
    ),
    names_to = "District",
    values_to = "population"
  ) |>
  group_by(District) |>
  summarise(
    average_deprivation = weighted.mean(
      index,
      population,
      na.rm = TRUE
    ),
    .groups = "drop"
  )

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# Dementia medication
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

dementia_data <- Data_ByChemical_clean |>
  filter(
    Chemical %in% dementia_chemicals,
    District != "Unknown"
  ) |>
  mutate(
    NumDisps = case_when(
      NumDisps == "<6" ~ 2.5,
      TRUE ~ as.numeric(NumDisps)
    ),
    NumPpl = case_when(
      NumPpl == "<6" ~ 2.5,
      TRUE ~ as.numeric(NumPpl)
    )
  ) |>
  group_by(YearDisp, District, Chemical) |>
  summarise(
    NumDisps = sum(NumDisps),
    NumPpl = sum(NumPpl),
    .groups = "drop"
  ) |>
  left_join(
    Deprivation_average,
    by = "District"
  )

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

# Create a glm for predicting the number of distributions based on year,
# average deprivation and chemical. Start with Poisson with interactions and no 
# random effects
dementia_model_1 <- dementia_data |>
  glm(
    NumDisps ~ factor(YearDisp) * average_deprivation + Chemical,
    family = poisson(link = "log"),
    data = _
    )

dementia_model_1 |> summary()

# ------------------------------------------------------------------------------
# Graphics
# ------------------------------------------------------------------------------

# Bar chart of the most dispersed drugs
Data_ByChemical_count |>
  slice_max(NumDisps, n = 25) |>
  mutate(Chemical = fct_reorder(Chemical, NumDisps)) |>
  ggplot(aes(x = NumDisps, y = Chemical)) +
  geom_col() +
  scale_x_continuous() +
  labs(
    title = "Bar chart of the most dispersed drugs",
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

# Convert deprivation back to long format for graphics
deprivation_long <- Deprivation_wide |>
  # Keep only deprivation categories 1–10
  filter(
    new_zealand_index_of_socioeconomic_deprivation %in%
      c(
        "1 - Least deprived",
        "2", "3", "4", "5", "6", "7", "8", "9",
        "10 - Most deprived"
      )
  ) |>
  # Convert the area columns into rows
  pivot_longer(
    cols = -new_zealand_index_of_socioeconomic_deprivation,
    names_to = "area",
    values_to = "count"
  ) |>
  group_by(area) |>
  mutate(
    proportion = count / sum(count)
  ) |>
  ungroup() |>
  # remove area instances that are more aggregated than health district
  filter(
    !area %in% c(
      "Total - New Zealand by health region/health district",
      "Central Region",
      "Northern Region",
      "Te Manawa Taki",
      "Te Waipounamu"
    )
  ) |>
  mutate(area = factor(area, levels = rev(district_order)))
         
# Heatmap of deprivation index across districts
ggplot(
  deprivation_long,
  aes(
    x = new_zealand_index_of_socioeconomic_deprivation,
    y = area,
    fill = proportion
  )
) +
  geom_tile(colour = "white") +
  scale_fill_viridis_c(
    labels = label_percent(accuracy = 1),
    name = "Population share"
  ) +
  labs(
    title = "Distribution of socioeconomic deprivation by area",
    x = "New Zealand Index of Socioeconomic Deprivation",
    y = NULL
  ) +
  theme(
    panel.grid = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )
