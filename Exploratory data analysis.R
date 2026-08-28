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
  scales,              # Graphics formatting
  MASS,                # Negative binomial models
  glmmTMB              # Negative binomial mixed models
)

# Set the seed
set.seed(111)

# Load the data
Data_ByChemical <- read.csv("Data/Data_ByChemical.csv") |>
  as_tibble()
Deprivation <- read.csv("Data/Deprivation index 2023.csv") |>
  as_tibble() |>
  clean_names()
Retired_population <- read.csv("Data/Retired population annual estimates.csv") |>
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
        NumDisps == "<6" ~ "3", 
        .default = NumDisps)
    ),
    NumPpl = as.numeric(
      case_when(
        NumPpl == "<6" ~ "3", 
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
# Suppression - THIS SECTION NO LONGER WORKS DUE TO THE IMPUTATION OCCURING
# EARLIER IN THE PROCESS
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

# # Check that the data is sufficiently suppressed
# Data_ByChemical_suppressed_counts <- Data_ByChemical |> 
#   mutate(
#     suppressed_disp = NumDisps == "<6",
#     suppressed_people = NumPpl == "<6"
#     ) |>
#   group_by(YearDisp, Chemical, Type) |>
#   summarise(
#     suppressed_people_count = sum(suppressed_people),
#     suppressed_disp_count = sum(suppressed_disp)
#     ) |>
#   ungroup()
# 
# Data_ByChemical_suppression_problems <- Data_ByChemical_suppressed_counts |>
#   filter(suppressed_disp_count == 1) |>
#   arrange(suppressed_disp_count)
# 
# # An example of YearDisp, Chemical and Type combination with 
# # insufficient suppression
# Example_problem_values <- Data_ByChemical_suppression_problems |>
#   head(1) |>
#   dplyr::select(YearDisp, Chemical, Type)
# 
# Data_ByChemical_suppression_problems_xmpl <- Data_ByChemical |>
#   filter(
#     YearDisp == pull(Example_problem_values, YearDisp),
#     Chemical == pull(Example_problem_values, Chemical),
#     Type == pull(Example_problem_values, Type)
#   )
# 
# # South Canterbury has the NumDisps attribute supressed with the value of "<6"
# # but the true value can be easily found with the following procedure
# Data_ByChemical_suppression_problems_xmpl |>
#   filter(District != "South Canterbury") |>
#   mutate(
#     NumDisps = as.numeric(NumDisps) + rbinom(n = n(), size = 1, prob = 0.5), # Add randomness so to not break any privacy rules
#     Is_total = District == "New Zealand"
#     ) |>
#   group_by(Is_total) |>
#   summarise(NumDisps = sum(NumDisps)) |>
#   ungroup() |>
#   pivot_wider(names_from = Is_total, values_from = NumDisps) |>
#   mutate(
#     # Account for the additions in NumDisps for each of the districts
#     `TRUE` = `TRUE` + (nrow(Data_ByChemical_suppression_problems_xmpl) - 1) / 2, 
#     Unsupressed_value = `TRUE` - `FALSE`
#     ) |>
#   pull(Unsupressed_value)
# 
# # Find the proportion of YearDisp, Chemical and Type combinations that have
# # insufficient suppressions
# Data_ByChemical_suppressed_counts |>
#   mutate(one_suppressed = suppressed_people_count == 1 | suppressed_disp_count == 1) |>
#   count(one_suppressed) |>
#   pivot_wider(names_from = one_suppressed, values_from = n) |>
#   mutate(proportion = `TRUE` / (`FALSE` + `TRUE`)) |>
#   pull(proportion)
# # [1] 0.1061375
# 
# # This means that roughly 11% of the combinations of all YearDisp, Chemical and
# # Type have suppressions that are insufficient.
# 
# # The reason that the having instances of counts less than 6 is problematic is 
# # that small values in these columns increase the risk of identifying someone
# # from a rare medicine as well as district and year.

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
  dplyr::select(area, new_zealand_index_of_socioeconomic_deprivation, obs_value) |>
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
      NumDisps == "<6" ~ 3,
      TRUE ~ as.numeric(NumDisps)
    ),
    NumPpl = case_when(
      NumPpl == "<6" ~ 3,
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

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# Population counts
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

# Check the categories
Retired_population |>
  distinct(
    age_popes_sub_002,
    age
  ) |>
  arrange(age_popes_sub_002)

# Check the years and areas
Retired_population |>
  summarise(
    first_year = min(year_at_30_june),
    last_year = max(year_at_30_june),
    number_of_areas = n_distinct(area)
  )

(Retired_population |>
  distinct(area) |>
  arrange(area))

# Tidy the data
Retired_population_clean <- Retired_population |>
  filter(
    age != "65 years and over",
    area != "Area outside health district"
    ) |>
  mutate(
    YearDisp = year_at_30_june,
    age_group = age,
    age_lower = parse_number(age),
    population = obs_value,
    District = area
  ) |>
  select(YearDisp, YearDisp, age_group, age_lower, District, population)

# Find the district populations for people aged 65 or over
population_65_plus <- Retired_population_clean |>
  group_by(
    YearDisp,
    District
  ) |>
  summarise(
    population_65_plus = sum(
      population
    )
  ) |>
  ungroup()

# Combine population and dispensing data
dementia_population <- dementia_data |>
  left_join(
    population_65_plus,
    by = c(
      "YearDisp",
      "District"
    )
  ) |>
  mutate(
    dispensings_per_1000_65plus =
      NumDisps / population_65_plus * 1000,
    
    people_per_1000_65plus =
      NumPpl / population_65_plus * 1000
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
      NumDisps == "<6" ~ 3,
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

dementia_model_data <- dementia_data |>
  mutate(
    # Keep year categorical because the trend may not be linear
    Year = factor(YearDisp),
    # Centre deprivation so model intercepts are easier to interpret
    deprivation_c =
      average_deprivation - mean(average_deprivation),
    Chemical = factor(Chemical),
    District = factor(District)
  )

# Model 0: differences between chemicals only
poisson_0 <- dementia_model_data |>
  glm(
    NumDisps ~ Chemical,
    family = poisson(link = "log"),
    data = _
    )

# Model 1: additive effects
poisson_1 <- dementia_model_data |>
  glm(
    NumDisps ~ Year + deprivation_c + Chemical,
    family = poisson(link = "log"),
    data = _
  )

# Model 2: deprivation effect can differ by year
poisson_2 <- dementia_model_data |>
  glm(
    NumDisps ~ Year * deprivation_c + Chemical,
    family = poisson(link = "log"),
    data = _
  )

# Model 3: time patterns can also differ by chemical
poisson_3 <- dementia_model_data |>
  glm(
    NumDisps ~
      Year * deprivation_c +
      Year * Chemical,
    family = poisson(link = "log"),
    data = _
  )

# Model 4: all two-way interactions
poisson_4 <- dementia_model_data |>
  glm(
    NumDisps ~
      Year * deprivation_c +
      Year * Chemical +
      deprivation_c * Chemical,
    family = poisson(link = "log"),
    data = _
  )

# Check Poisson overdispersion
check_poisson_dispersion <- function(model) {
  pearson_dispersion <- sum(residuals(model, type = "pearson")^2) / df.residual(model)
  
  tibble(
    residual_deviance = deviance(model),
    residual_df = df.residual(model),
    deviance_ratio = deviance(model) / df.residual(model),
    pearson_dispersion = pearson_dispersion
  )
}

check_poisson_dispersion(poisson_0)
check_poisson_dispersion(poisson_1)
check_poisson_dispersion(poisson_2)
check_poisson_dispersion(poisson_3)
check_poisson_dispersion(poisson_4)
# a Pearson dispersion of roughly 1 would indicate that the Poisson assumption
# of the mean equalling the variance is appropriate. All of these models have a
# Pearson dispersion well above 1000, indicating that there is substantial
# overdispersion for a Poisson model

# Additive negative binomial model
nb_1 <- dementia_model_data |>
  glm.nb(NumDisps ~ Year + deprivation_c + Chemical, data = _)

# Year-by-deprivation interaction
nb_2 <- dementia_model_data |>
  glm.nb(NumDisps ~ Year * deprivation_c + Chemical, data = _)

# Year trends can differ by chemical
nb_3 <- dementia_model_data |>
  glm.nb(NumDisps ~
           Year * deprivation_c +
           Year * Chemical, data = _)

# All substantively useful two-way interactions
nb_4 <- dementia_model_data |>
  glm.nb(NumDisps ~
           Year * deprivation_c +
           Year * Chemical +
           deprivation_c * Chemical,
         data = _)

# Poisson mixed model
mixed_poisson <- dementia_model_data |>
  glmmTMB(
    NumDisps ~
      Year * deprivation_c +
      Chemical +
      (1 | District),
    family = poisson(link = "log"),
    data = _
  )

# Negative binomial mixed model
mixed_nb_1 <- dementia_model_data |>
  glmmTMB(
    NumDisps ~
      Year * deprivation_c +
      Chemical +
      (1 | District),
    family = nbinom2(link = "log"),
    data = _
  )

# Allow chemical-specific time patterns
mixed_nb_2 <- dementia_model_data |>
  glmmTMB(
    NumDisps ~
      Year * deprivation_c +
      Year * Chemical +
      (1 | District),
    family = nbinom2(link = "log"),
    data = _
  )

# Include all relevant two-way interactions
mixed_nb_3 <- dementia_model_data |>
  glmmTMB(
    NumDisps ~
      Year * deprivation_c +
      Year * Chemical +
      deprivation_c * Chemical +
      (1 | District),
    family = nbinom2(link = "log"),
    data = _
  )

BIC(
  poisson_0,
  poisson_1,
  poisson_2,
  poisson_3,
  poisson_4,
  nb_1,
  nb_2,
  nb_3,
  nb_4,
  mixed_poisson,
  mixed_nb_1,
  mixed_nb_2,
  mixed_nb_3
) |>
  as.data.frame() |>
  rownames_to_column("model") |>
  as_tibble() |>
  arrange(BIC)

# The mixed negative binomial models have the lowest BIC, but many of the
# coefficients are not significantly different from 0.

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

# Population size against dispensing count
ggplot(
  dementia_population,
  aes(
    x = population_65_plus,
    y = NumDisps,
    shape = factor(YearDisp),
    colour = factor(YearDisp)
  )
) +
  geom_point() +
  geom_smooth(
    aes(group = 1),
    method = "lm",
    se = TRUE,
    colour = "black",
    fill = "grey",
    linetype = "dashed"
  ) +
  facet_wrap(
    ~ Chemical,
    scales = "free_y"
  ) +
  scale_x_continuous(
    labels = label_comma()
  ) +
  scale_y_continuous(
    labels = label_comma()
  ) +
  labs(
    title = "Dementia-medication dispensing and the population aged 65+",
    x = "District population aged 65 and older",
    y = "Number of dispensings",
    shape = "Year",
    colour = "Year"
  )

# 2024 Dispensing rates by district
dementia_population |>
  filter(YearDisp == 2024) |>
  ggplot(
    aes(
      x = dispensings_per_1000_65plus,
      y = factor(
        District,
        levels = rev(district_order)
      )
    )
  ) +
  geom_col(
    fill = "grey"
  ) +
  facet_wrap(
    ~ Chemical,
    scales = "free_x"
  ) +
  labs(
    title = "Dementia-medication dispensing rates by district in 2024",
    x = "Dispensings per 1,000 people aged 65+",
    y = NULL
  )

# Scatterplot showing dispensing rate by average deprivation for each year and
# drug
ggplot(
  dementia_population,
  aes(
    x = average_deprivation,
    y = dispensings_per_1000_65plus
  )
) +
  geom_point() +
  geom_smooth(
    method = "lm",
    se = TRUE,
    colour = "red",
    fill = "grey"
  ) +
  facet_grid(
    Chemical ~ YearDisp,
    scales = "free_y"
  ) +
  labs(
    title = "Deprivation and dementia medication dispensing rates",
    x = "Average deprivation",
    y = "Dispensings per 1,000 people aged 65+"
  )
