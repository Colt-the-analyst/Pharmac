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

sum(pharms$NumDisps == "<6")
sum(pharms$NumPpl == "<6")

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
# They are converted to NA rather than being treated as zero.

dementia_drugs <- dementia_drugs %>%
  mutate(
    YearDisp = as.integer(YearDisp),
    NumDisps_num = as.numeric(na_if(NumDisps, "<6")),
    NumPpl_num = as.numeric(na_if(NumPpl, "<6"))
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

ggplot(
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

# --------------------------------------------------
# EDA 8: District-level analysis
# --------------------------------------------------

# Remove the New Zealand national total so it is not
# treated as an individual district.

district_data <- dispensing_data %>%
  filter(District != "New Zealand")

district_2024 <- district_data %>%
  filter(YearDisp == 2024)

district_2024 %>%
  select(
    Chemical,
    District,
    NumDisps_num
  ) %>%
  arrange(Chemical, desc(NumDisps_num)) %>%
  print(n = 40)

ggplot(
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

# --------------------------------------------------
# EDA 9: Suppression in selected drugs
# --------------------------------------------------

dementia_drugs %>%
  group_by(Chemical, Type) %>%
  summarise(
    suppressed_dispensings = sum(is.na(NumDisps_num)),
    suppressed_people = sum(is.na(NumPpl_num)),
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
  geom_line(na.rm = TRUE) +
  geom_point(na.rm = TRUE) +
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
    percent_change =
      ((year_2024 - year_2020) /
         year_2020) * 100
  )

district_change %>%
  arrange(Chemical, desc(percent_change)) %>%
  print(n = 40)