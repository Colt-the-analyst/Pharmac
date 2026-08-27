library(tidyverse)

# --------------------------------------------------
# Load data
# --------------------------------------------------

pharms <- read_csv(
  "Data_ByChemical.csv",
  col_types = cols(.default = col_character())
)

dim(pharms)
names(pharms)
glimpse(pharms)
head(pharms)

# --------------------------------------------------
# EDA 1: Check years, districts and dispensing types
# --------------------------------------------------

sort(unique(pharms$YearDisp))
sort(unique(pharms$District))
unique(pharms$Type)

# Number of different chemicals
n_distinct(pharms$Chemical)

# Check for duplicate rows
sum(duplicated(pharms))

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

dim(dementia_drugs)

# Number of observations for each drug
dementia_drugs %>%
  count(Chemical)

# Chemical IDs
dementia_drugs %>%
  distinct(Chemical, ChemID)

# Check years available
dementia_drugs %>%
  count(YearDisp)

# Check districts available
sort(unique(dementia_drugs$District))

# Check dispensing types
dementia_drugs %>%
  count(Chemical, Type)

# --------------------------------------------------
# EDA 4: Clean numeric variables
# --------------------------------------------------

# Values recorded as "<6" are suppressed small counts.
# The true value is between 1 and 5.
# Suppressed values are imputed as 3, the midpoint
# of the possible range.

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

glimpse(dementia_drugs)

# --------------------------------------------------
# EDA 5: Select total dispensings
# --------------------------------------------------

dispensing_data <- dementia_drugs %>%
  filter(Type == "Dispensings")

dim(dispensing_data)

# --------------------------------------------------
# EDA 6: National trends
# --------------------------------------------------

national <- dispensing_data %>%
  filter(District == "New Zealand")

national %>%
  select(
    Chemical,
    YearDisp,
    NumDisps_num,
    NumPpl_num
  ) %>%
  arrange(Chemical, YearDisp)

# --------------------------------------------------
# EDA 7: National dispensing trend graph
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
    title = "National dispensing trends, 2020–2024",
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

# --------------------------------------------------
# EDA 8: District-level analysis
# --------------------------------------------------

district_data <- dispensing_data %>%
  filter(District != "New Zealand")

district_2024 <- district_data %>%
  filter(YearDisp == 2024)

district_2024_plot <- ggplot(
  district_2024,
  aes(
    x = reorder(District, NumDisps_num),
    y = NumDisps_num
  )
) +
  geom_col() +
  coord_flip() +
  facet_wrap(~ Chemical, scales = "free_y") +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title = "Dispensings by health district in 2024",
    x = "Health district",
    y = "Number of dispensings"
  ) +
  theme_minimal()

district_2024_plot

ggsave(
  "district_dispensing_2024.png",
  district_2024_plot,
  width = 10,
  height = 7,
  dpi = 300
)

# --------------------------------------------------
# EDA 9: Suppression in selected drugs
# --------------------------------------------------

# Count how many observations were originally suppressed.

dementia_drugs %>%
  group_by(Chemical, Type) %>%
  summarise(
    suppressed_dispensings =
      sum(NumDisps_suppressed, na.rm = TRUE),
    
    suppressed_people =
      sum(NumPpl_suppressed, na.rm = TRUE),
    
    total_observations = n(),
    
    percent_dispensings_suppressed =
      mean(NumDisps_suppressed, na.rm = TRUE) * 100,
    
    percent_people_suppressed =
      mean(NumPpl_suppressed, na.rm = TRUE) * 100,
    
    .groups = "drop"
  )

# --------------------------------------------------
# EDA 10: Percentage change from 2020 to 2024
# --------------------------------------------------

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
# EDA 11: District trends over time
# --------------------------------------------------

ggplot(
  district_data,
  aes(
    x = YearDisp,
    y = NumDisps_num,
    group = District
  )
) +
  geom_line() +
  geom_point() +
  facet_wrap(~ Chemical, scales = "free_y") +
  scale_x_continuous(breaks = 2020:2024) +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title = "District dispensing trends, 2020–2024",
    x = "Year",
    y = "Number of dispensings"
  ) +
  theme_minimal()

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
    absolute_change =
      year_2024 - year_2020,
    
    percent_change =
      ((year_2024 - year_2020) /
         year_2020) * 100
  )

district_change %>%
  arrange(Chemical, desc(percent_change)) %>%
  print(n = 40)

# --------------------------------------------------
# QA 1: Missing values after cleaning
# --------------------------------------------------

sum(is.na(dementia_drugs$NumDisps_num))
sum(is.na(dementia_drugs$NumPpl_num))


# --------------------------------------------------
# QA 2: Duplicate rows
# --------------------------------------------------

sum(duplicated(dementia_drugs))


# --------------------------------------------------
# QA 3: Check years and districts
# --------------------------------------------------

sort(unique(dementia_drugs$YearDisp))
n_distinct(district_data$District)
sort(unique(district_data$District))

# --------------------------------------------------
# QA 4: Check unique drug-district-year combinations
# --------------------------------------------------

district_data %>%
  count(Chemical, District, YearDisp) %>%
  filter(n > 1)


# --------------------------------------------------
# Report figure: District change 2020 to 2024
# --------------------------------------------------

district_change_plot <- ggplot(
  district_change,
  aes(
    x = reorder(District, percent_change),
    y = percent_change
  )
) +
  geom_col() +
  coord_flip() +
  facet_wrap(~ Chemical, scales = "free_y") +
  labs(
    title = "Change in dispensings by district, 2020–2024",
    x = "Health district",
    y = "Percentage change (%)"
  ) +
  theme_minimal()

district_change_plot

ggsave(
  "district_change_2020_2024.png",
  district_change_plot,
  width = 10,
  height = 7,
  dpi = 300
)


# --------------------------------------------------
# Report table: National change
# --------------------------------------------------

report_table <- national_change %>%
  transmute(
    Drug = Chemical,
    `Dispensings 2020` = NumDisps_num_2020,
    `Dispensings 2024` = NumDisps_num_2024,
    `Change (%)` = round(dispensing_change_percent, 1)
  )

report_table
write_csv(
  report_table,
  "national_change_table.csv"
)