library(tidyverse)

# --------------------------------------------------
# Load data
# --------------------------------------------------

pharms <- read_csv(
  "Data_ByChemical.csv",
  col_types = cols(.default = col_character())
)

# Basic structure checks are useful for QA, but are not intended
# to be a main focus of the written report.
dim(pharms)
names(pharms)
glimpse(pharms)

# --------------------------------------------------
# EDA 1: Dataset coverage and dispensing types
# --------------------------------------------------

sort(unique(pharms$YearDisp))
sort(unique(pharms$District))
unique(pharms$Type)
n_distinct(pharms$Chemical)
sum(duplicated(pharms))

# The dataset contains two Type values:
# - "Initial dispensings": excludes repeat dispensings.
# - "Dispensings": includes repeat dispensings.
# The report focuses on "Dispensings" because the research question
# is about total dispensing activity rather than only initial supplies.

pharms %>%
  count(Type)

# --------------------------------------------------
# EDA 2: Check suppressed small counts
# --------------------------------------------------

sum(pharms$NumDisps == "<6", na.rm = TRUE)
sum(pharms$NumPpl == "<6", na.rm = TRUE)

# --------------------------------------------------
# EDA 3: Select pharmaceuticals of interest
# --------------------------------------------------

dementia_drugs <- pharms %>%
  filter(
    Chemical %in% c(
      "Donepezil hydrochloride",
      "Rivastigmine"
    )
  )

# Keep row counts as QA only; the report will focus on dispensing totals.
dim(dementia_drugs)

dementia_drugs %>%
  distinct(Chemical, ChemID)

dementia_drugs %>%
  count(Chemical, Type)

# --------------------------------------------------
# EDA 4: Clean numeric variables
# --------------------------------------------------

# Values recorded as "<6" are suppressed small counts.
# The true value is between 1 and 5.
# Suppressed values are imputed as 3, the midpoint of the range.
# Indicators are retained so the imputed observations remain identifiable.

dementia_drugs <- dementia_drugs %>%
  mutate(
    YearDisp = as.integer(YearDisp),

    NumDisps_suppressed = NumDisps == "<6",
    NumPpl_suppressed = NumPpl == "<6",

    NumDisps_num = as.numeric(
      replace(NumDisps, NumDisps == "<6", "3")
    ),

    NumPpl_num = as.numeric(
      replace(NumPpl, NumPpl == "<6", "3")
    )
  )

# --------------------------------------------------
# EDA 5: Understand Type values and focus on dispensing totals
# --------------------------------------------------

# Compare the two Type categories at the national level without
# double-counting district and New Zealand totals together.
type_summary_national <- dementia_drugs %>%
  filter(District == "New Zealand") %>%
  group_by(Chemical, Type) %>%
  summarise(
    total_num_disps_2020_2024 = sum(NumDisps_num, na.rm = TRUE),
    .groups = "drop"
  )

type_summary_national

# Main analysis uses total dispensings (includes repeats).
dispensing_data <- dementia_drugs %>%
  filter(Type == "Dispensings")

# --------------------------------------------------
# EDA 6: National trends and detailed national table
# --------------------------------------------------

national <- dispensing_data %>%
  filter(District == "New Zealand")

national_table <- national %>%
  select(
    Chemical,
    YearDisp,
    NumDisps_num,
    NumPpl_num
  ) %>%
  arrange(Chemical, YearDisp)

national_table

write_csv(
  national_table,
  "national_dispensing_people_table.csv"
)

# Aggregated view across the two selected drugs.
# NumPpl is NOT summed here because a person could appear in both drug groups.
national_aggregated <- national %>%
  group_by(YearDisp) %>%
  summarise(
    total_dispensings = sum(NumDisps_num, na.rm = TRUE),
    .groups = "drop"
  )

national_aggregated

# --------------------------------------------------
# EDA 7: National dispensing trend graphs
# --------------------------------------------------

national_trend_plot <- ggplot(
  national,
  aes(
    x = YearDisp,
    y = NumDisps_num,
    group = Chemical
  )
) +
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
  facet_wrap(~ Chemical, scales = "free_y") +
  scale_x_continuous(breaks = 2020:2024) +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title = "National dispensing trends, 2020-2024",
    x = "Year",
    y = "Number of dispensings"
  ) +
  theme_minimal()

national_trend_plot

ggsave(
  "national_dispensing_trends.png",
  national_trend_plot,
  width = 10,
  height = 6,
  dpi = 300
)

aggregated_national_plot <- ggplot(
  national_aggregated,
  aes(
    x = YearDisp,
    y = total_dispensings
  )
) +
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
  scale_x_continuous(breaks = 2020:2024) +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title = "Combined national dispensings for the two selected drugs",
    x = "Year",
    y = "Total dispensings"
  ) +
  theme_minimal()

aggregated_national_plot

ggsave(
  "national_dispensing_aggregated.png",
  aggregated_national_plot,
  width = 8,
  height = 5,
  dpi = 300
)

# --------------------------------------------------
# EDA 8: District-level dispensing in 2024
# --------------------------------------------------

district_data <- dispensing_data %>%
  filter(District != "New Zealand")

district_2024 <- district_data %>%
  filter(YearDisp == 2024)

# Create a within-drug ordering for the district bars.
district_2024_plot_data <- district_2024 %>%
  arrange(Chemical, NumDisps_num) %>%
  mutate(
    District_plot = paste(Chemical, District, sep = "___"),
    District_plot = factor(District_plot, levels = unique(District_plot))
  )

# Free x-axis scales allow the distribution within each drug to be visible.
district_2024_plot <- ggplot(
  district_2024_plot_data,
  aes(
    x = NumDisps_num,
    y = District_plot
  )
) +
  geom_col() +
  facet_wrap(~ Chemical, scales = "free") +
  scale_x_continuous(labels = scales::comma) +
  scale_y_discrete(
    labels = function(x) sub("^.*___", "", x)
  ) +
  labs(
    title = "Dispensings by health district in 2024",
    x = "Number of dispensings",
    y = "Health district"
  ) +
  theme_minimal()

district_2024_plot

ggsave(
  "district_dispensing_2024_free_x.png",
  district_2024_plot,
  width = 11,
  height = 7,
  dpi = 300
)

# EDA 8B: Simple two-bar comparison requested in feedback.
drug_2024_totals <- national %>%
  filter(YearDisp == 2024) %>%
  select(Chemical, NumDisps_num)

drug_2024_totals

write_csv(
  drug_2024_totals,
  "drug_2024_totals.csv"
)

drug_2024_plot <- ggplot(
  drug_2024_totals,
  aes(
    x = Chemical,
    y = NumDisps_num
  )
) +
  geom_col() +
  geom_text(
    aes(label = scales::comma(NumDisps_num)),
    vjust = -0.4
  ) +
  scale_y_continuous(
    labels = scales::comma,
    expand = expansion(mult = c(0, 0.12))
  ) +
  labs(
    title = "National dispensings by selected drug in 2024",
    x = NULL,
    y = "Number of dispensings"
  ) +
  theme_minimal()

drug_2024_plot

ggsave(
  "drug_dispensing_comparison_2024.png",
  drug_2024_plot,
  width = 7,
  height = 5,
  dpi = 300
)

# --------------------------------------------------
# EDA 8C: Population context and population-adjusted rates
# --------------------------------------------------

# Stats NZ population estimates are available by health district, year,
# age and sex. For this dementia-medication analysis, use the population
# aged 65 years and over as the denominator. Rates are therefore expressed
# as dispensings per 10,000 people aged 65+.
#
# Important geography note:
# The population file combines Capital & Coast and Hutt Valley into
# "Capital, Coast and Hutt Valley". To make the datasets comparable,
# dispensing counts for those two pharmaceutical districts are combined
# only for the population-adjusted analysis. The original district graph
# above still reports the 20 pharmaceutical districts separately.

population_raw <- read_csv(
  "Retired population annual estimates.csv",
  col_types = cols(.default = col_character())
)

population_65plus <- population_raw %>%
  transmute(
    YearDisp = as.integer(`Year at 30 June`),
    Sex = Sex,
    Age = Age,
    District_pop = case_when(
      Area == "Tairāwhiti" ~ "Tairawhiti",
      Area == "Waitematā" ~ "Waitemata",
      TRUE ~ Area
    ),
    Population_65plus = as.numeric(OBS_VALUE)
  ) %>%
  filter(
    YearDisp %in% 2020:2024,
    Sex == "Total people, sex",
    Age == "65 years and over",
    District_pop != "Area outside health district"
  ) %>%
  select(
    YearDisp,
    District = District_pop,
    Population_65plus
  )

population_65plus

write_csv(
  population_65plus,
  "population_65plus_health_districts_2020_2024.csv"
)

# Harmonise pharmaceutical geography with the population geography.
# Capital & Coast and Hutt Valley must be combined because the population
# source does not provide them separately.
district_for_population <- district_data %>%
  mutate(
    District = case_when(
      District %in% c("Capital & Coast", "Hutt Valley") ~
        "Capital, Coast and Hutt Valley",
      TRUE ~ District
    )
  ) %>%
  group_by(
    Chemical,
    District,
    YearDisp
  ) %>%
  summarise(
    NumDisps_num = sum(NumDisps_num, na.rm = TRUE),
    .groups = "drop"
  )

# Check that all pharmaceutical geographies match a population geography.
unmatched_population_districts <- district_for_population %>%
  distinct(District) %>%
  anti_join(
    population_65plus %>% distinct(District),
    by = "District"
  )

unmatched_population_districts

# Join population estimates and calculate population-adjusted dispensing rates.
district_population_rates <- district_for_population %>%
  left_join(
    population_65plus,
    by = c("District", "YearDisp")
  ) %>%
  mutate(
    dispensings_per_10000_65plus =
      (NumDisps_num / Population_65plus) * 10000
  )

district_population_rates

write_csv(
  district_population_rates,
  "district_dispensing_rates_65plus_2020_2024.csv"
)

# 2024 rate table for the report.
district_rates_2024 <- district_population_rates %>%
  filter(YearDisp == 2024) %>%
  arrange(Chemical, desc(dispensings_per_10000_65plus))

district_rates_2024

write_csv(
  district_rates_2024,
  "district_dispensing_rates_65plus_2024.csv"
)

# Population-adjusted 2024 graph. Free x-axis scales are used so that
# variation within each medication remains visible.
district_rates_2024_plot_data <- district_rates_2024 %>%
  arrange(Chemical, dispensings_per_10000_65plus) %>%
  mutate(
    District_plot = paste(Chemical, District, sep = "___"),
    District_plot = factor(District_plot, levels = unique(District_plot))
  )

district_rates_2024_plot <- ggplot(
  district_rates_2024_plot_data,
  aes(
    x = dispensings_per_10000_65plus,
    y = District_plot
  )
) +
  geom_col() +
  facet_wrap(~ Chemical, scales = "free") +
  scale_x_continuous(labels = scales::comma) +
  scale_y_discrete(
    labels = function(x) sub("^.*___", "", x)
  ) +
  labs(
    title = "Dispensings per 10,000 people aged 65+ by health district, 2024",
    subtitle = "Capital & Coast and Hutt Valley are combined to match the population geography",
    x = "Dispensings per 10,000 people aged 65+",
    y = "Health district"
  ) +
  theme_minimal()

district_rates_2024_plot

ggsave(
  "district_dispensing_rates_65plus_2024.png",
  district_rates_2024_plot,
  width = 11,
  height = 7,
  dpi = 300
)

# --------------------------------------------------
# EDA 9: Suppression in selected drugs
# --------------------------------------------------

suppression_summary <- dementia_drugs %>%
  group_by(Chemical, Type) %>%
  summarise(
    suppressed_dispensings = sum(NumDisps_suppressed, na.rm = TRUE),
    suppressed_people = sum(NumPpl_suppressed, na.rm = TRUE),
    total_observations = n(),
    percent_dispensings_suppressed =
      mean(NumDisps_suppressed, na.rm = TRUE) * 100,
    percent_people_suppressed =
      mean(NumPpl_suppressed, na.rm = TRUE) * 100,
    .groups = "drop"
  )

suppression_summary

# --------------------------------------------------
# EDA 10: Annual national change across all years
# --------------------------------------------------

national_annual_change <- national %>%
  arrange(Chemical, YearDisp) %>%
  group_by(Chemical) %>%
  mutate(
    previous_year = lag(YearDisp),
    previous_dispensings = lag(NumDisps_num),
    absolute_change = NumDisps_num - previous_dispensings,
    annual_change_percent =
      ((NumDisps_num - previous_dispensings) /
         previous_dispensings) * 100
  ) %>%
  ungroup() %>%
  filter(!is.na(previous_year)) %>%
  mutate(
    period = paste0(previous_year, "-", YearDisp)
  )

national_annual_change

annual_change_report_table <- national_annual_change %>%
  select(
    Chemical,
    period,
    absolute_change,
    annual_change_percent
  ) %>%
  mutate(
    annual_change_percent = round(annual_change_percent, 1)
  )

annual_change_report_table

write_csv(
  annual_change_report_table,
  "national_annual_change_table.csv"
)

annual_change_plot <- ggplot(
  national_annual_change,
  aes(
    x = period,
    y = annual_change_percent,
    group = Chemical
  )
) +
  geom_hline(yintercept = 0, linewidth = 0.4) +
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
  facet_wrap(~ Chemical, scales = "free_y") +
  labs(
    title = "Annual change in national dispensings",
    x = "Period",
    y = "Change from previous year (%)"
  ) +
  theme_minimal()

annual_change_plot

ggsave(
  "national_annual_change.png",
  annual_change_plot,
  width = 9,
  height = 5,
  dpi = 300
)

# Keep the overall 2020-2024 comparison as a supporting summary.
national_change <- national %>%
  filter(YearDisp %in% c(2020, 2024)) %>%
  select(
    Chemical,
    YearDisp,
    NumDisps_num,
    NumPpl_num
  ) %>%
  pivot_wider(
    names_from = YearDisp,
    values_from = c(NumDisps_num, NumPpl_num)
  ) %>%
  mutate(
    dispensing_change_percent =
      ((NumDisps_num_2024 - NumDisps_num_2020) /
         NumDisps_num_2020) * 100,
    people_change_percent =
      ((NumPpl_num_2024 - NumPpl_num_2020) /
         NumPpl_num_2020) * 100
  )

national_change

# --------------------------------------------------
# EDA 11: District trends over time - less cluttered view
# --------------------------------------------------

# The previous 20-line-per-drug graph was difficult to read.
# This heatmap indexes each district to its own 2020 value (= 100),
# making relative change over time easier to compare across districts.
district_index <- district_data %>%
  group_by(Chemical, District) %>%
  arrange(YearDisp, .by_group = TRUE) %>%
  mutate(
    baseline_2020 = NumDisps_num[YearDisp == 2020][1],
    index_2020 = (NumDisps_num / baseline_2020) * 100
  ) %>%
  ungroup()

district_trend_heatmap <- ggplot(
  district_index,
  aes(
    x = factor(YearDisp),
    y = District,
    fill = index_2020
  )
) +
  geom_tile() +
  facet_wrap(~ Chemical) +
  scale_fill_viridis_c(
    name = "Index\n(2020 = 100)"
  ) +
  labs(
    title = "Relative district dispensing change over time",
    x = "Year",
    y = "Health district"
  ) +
  theme_minimal()

district_trend_heatmap

ggsave(
  "district_trends_heatmap.png",
  district_trend_heatmap,
  width = 11,
  height = 8,
  dpi = 300
)

# --------------------------------------------------
# EDA 12: District change from 2020 to 2024
# --------------------------------------------------

district_change <- district_data %>%
  filter(YearDisp %in% c(2020, 2024)) %>%
  select(
    Chemical,
    District,
    YearDisp,
    NumDisps_num
  ) %>%
  pivot_wider(
    names_from = YearDisp,
    values_from = NumDisps_num,
    names_prefix = "year_"
  ) %>%
  mutate(
    absolute_change = year_2024 - year_2020,
    percent_change =
      ((year_2024 - year_2020) /
         year_2020) * 100
  )

# Full table for appendix / further investigation.
district_change_full <- district_change %>%
  arrange(Chemical, desc(percent_change))

district_change_full %>%
  print(n = 40)

write_csv(
  district_change_full,
  "district_change_full_appendix.csv"
)

# Focused table for the main report:
# largest percentage increase, largest percentage decrease,
# and largest absolute increase for each drug.
district_change_highlights <- bind_rows(
  district_change %>%
    group_by(Chemical) %>%
    slice_max(percent_change, n = 1, with_ties = FALSE) %>%
    mutate(Highlight = "Largest percentage increase"),

  district_change %>%
    group_by(Chemical) %>%
    slice_min(percent_change, n = 1, with_ties = FALSE) %>%
    mutate(Highlight = "Largest percentage decrease"),

  district_change %>%
    filter(absolute_change > 0) %>%
    group_by(Chemical) %>%
    slice_max(absolute_change, n = 1, with_ties = FALSE) %>%
    mutate(Highlight = "Largest absolute increase")
) %>%
  ungroup() %>%
  distinct(
    Chemical,
    District,
    .keep_all = TRUE
  ) %>%
  select(
    Chemical,
    District,
    year_2020,
    year_2024,
    absolute_change,
    percent_change,
    Highlight
  ) %>%
  arrange(Chemical, Highlight)

district_change_highlights

write_csv(
  district_change_highlights,
  "district_change_highlights.csv"
)

# --------------------------------------------------
# QA checks
# --------------------------------------------------

# Missing values after cleaning
sum(is.na(dementia_drugs$NumDisps_num))
sum(is.na(dementia_drugs$NumPpl_num))

# Duplicate rows
sum(duplicated(dementia_drugs))

# Years and districts
sort(unique(dementia_drugs$YearDisp))
n_distinct(district_data$District)
sort(unique(district_data$District))

# Unique drug-district-year combinations within the selected Type.
district_data %>%
  count(Chemical, District, YearDisp) %>%
  filter(n > 1)

# Population join QA: both should return 0 / no rows.
sum(is.na(district_population_rates$Population_65plus))
unmatched_population_districts

# --------------------------------------------------
# Report tables
# --------------------------------------------------

national_overall_change_table <- national_change %>%
  transmute(
    Drug = Chemical,
    `Dispensings 2020` = NumDisps_num_2020,
    `Dispensings 2024` = NumDisps_num_2024,
    `Overall change (%)` = round(dispensing_change_percent, 1)
  )

national_overall_change_table

write_csv(
  national_overall_change_table,
  "national_overall_change_table.csv"
)
