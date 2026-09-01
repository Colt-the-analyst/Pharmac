#Exploratory data analysis:

library(tidyverse)
library(janitor)
library(scales)

#loading the data:

chemical <- read.csv("Data_ByChemical.csv")
tg2 <- read.csv("Data_ByTG2.csv")
tg3 <- read.csv("Data_ByTG3.csv")
lookup <- read.csv("PharmaceuticalsLookup.csv")

dementia_chemicals <- c(
  "Donepezil hydrochloride",
  "Rivastigmine"
)

#initial data exploration:

dim(chemical)
dim(tg2)
dim(tg3)
dim(lookup)

names(chemical)
names(tg2)
names(tg3)
names(lookup)

head(chemical)
str(chemical)
summary(chemical)

# Missing values
colSums(is.na(chemical))

# Duplicate rows
sum(duplicated(chemical))

# These are row counts, not dispensing totals
table(chemical$YearDisp)
table(chemical$District)
table(chemical$Type)

n_distinct(chemical$Chemical)

#Suppression analysis before replacing <6 with 3

chemical <- chemical %>%
  mutate(
    DispensingCountStatus = ifelse(
      NumDisps == "<6",
      "Suppressed count (<6)",
      "Reported exact count"
    ),
    PeopleCountStatus = ifelse(
      NumPpl == "<6",
      "Suppressed count (<6)",
      "Reported exact count"
    )
  )

table(chemical$DispensingCountStatus)
table(chemical$PeopleCountStatus)

suppression_summary <- chemical %>%
  summarise(
    PercentDispensingRecordsSuppressed =
      mean(NumDisps == "<6") * 100,
    
    PercentPeopleCountRecordsSuppressed =
      mean(NumPpl == "<6") * 100
  )

suppression_summary

#Suppression status plot

suppression_plot_data <- chemical %>%
  count(DispensingCountStatus)

ggplot(
  suppression_plot_data,
  aes(
    x = DispensingCountStatus,
    y = n
  )
) +
  geom_col() +
  scale_y_continuous(labels = comma) +
  labs(
    title = "Suppression Status of Dispensing Records",
    subtitle = "Suppressed records contain dispensing counts below 6",
    x = NULL,
    y = "Number of Records"
  ) +
  theme_minimal()

#Suppression rate by year

suppression_year <- chemical %>%
  group_by(YearDisp) %>%
  summarise(
    Records = n(),
    
    SuppressedRecords =
      sum(NumDisps == "<6"),
    
    SuppressionRate =
      SuppressedRecords / Records * 100,
    
    .groups = "drop"
  )

suppression_year

#Suppression rate by district

suppression_district <- chemical %>%
  filter(
    District != "New Zealand"
  ) %>%
  group_by(District) %>%
  summarise(
    Records = n(),
    
    SuppressedRecords =
      sum(NumDisps == "<6"),
    
    SuppressionRate =
      SuppressedRecords / Records * 100,
    
    .groups = "drop"
  ) %>%
  arrange(
    desc(SuppressionRate)
  )

suppression_district

ggplot(
  suppression_district,
  aes(
    x = reorder(
      District,
      SuppressionRate
    ),
    y = SuppressionRate
  )
) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Suppression Rate by District",
    x = "District",
    y = "Records with Suppressed Dispensing Counts (%)"
  ) +
  theme_minimal()

#REPLACEING <6 WITH 3

chemical_clean <- chemical %>%
  mutate(
    NumDisps = ifelse(
      NumDisps == "<6",
      3,
      as.numeric(NumDisps)
    ),
    
    NumPpl = ifelse(
      NumPpl == "<6",
      3,
      as.numeric(NumPpl)
    )
  )

tg2_clean <- tg2 %>%
  mutate(
    NumDisps = ifelse(
      NumDisps == "<6",
      3,
      as.numeric(NumDisps)
    ),
    
    NumPpl = ifelse(
      NumPpl == "<6",
      3,
      as.numeric(NumPpl)
    )
  )

tg3_clean <- tg3 %>%
  mutate(
    NumDisps = ifelse(
      NumDisps == "<6",
      3,
      as.numeric(NumDisps)
    ),
    
    NumPpl = ifelse(
      NumPpl == "<6",
      3,
      as.numeric(NumPpl)
    )
  )
#Checking conversion
str(chemical_clean$NumDisps)
str(chemical_clean$NumPpl)

#DEMENTIA MEDICATION TABLE BY YEAR

dementia_table <- chemical_clean %>%
  filter(
    District == "New Zealand",
    Chemical %in% dementia_chemicals
  ) %>%
  group_by(
    YearDisp,
    Chemical
  ) %>%
  summarise(
    Dispensings = sum(NumDisps),
    People = sum(NumPpl),
    .groups = "drop"
  ) %>%
  pivot_wider(
    names_from = Chemical,
    values_from = c(
      Dispensings,
      People
    )
  ) %>%
  select(
    YearDisp,
    `Dispensings_Donepezil hydrochloride`,
    `People_Donepezil hydrochloride`,
    `Dispensings_Rivastigmine`,
    `People_Rivastigmine`
  ) %>%
  rename(
    Year = YearDisp,
    
    `Donepezil dispensings` =
      `Dispensings_Donepezil hydrochloride`,
    
    `Donepezil people` =
      `People_Donepezil hydrochloride`,
    
    `Rivastigmine dispensings` =
      `Dispensings_Rivastigmine`,
    
    `Rivastigmine people` =
      `People_Rivastigmine`
  )

dementia_table

#PHARMACEUTICAL RANKINGS

drug_rankings <- chemical_clean %>%
  filter(
    District == "New Zealand"
  ) %>%
  group_by(Chemical) %>%
  summarise(
    TotalDisp =
      sum(NumDisps),
    
    .groups = "drop"
  ) %>%
  arrange(
    desc(TotalDisp)
  ) %>%
  mutate(
    Rank = row_number()
  )
#Top 15 pharmaceuticals
top_drugs <- drug_rankings %>%
  slice_head(
    n = 15
  )

top_drugs

#Top 15 graph

ggplot(
  top_drugs,
  aes(
    x = reorder(
      Chemical,
      TotalDisp
    ),
    y = TotalDisp
  )
) +
  geom_col() +
  coord_flip() +
  scale_y_continuous(
    labels = comma
  ) +
  labs(
    title = "Top 15 Pharmaceuticals by Dispensing",
    x = "Pharmaceutical",
    y = "Number of Dispensings"
  ) +
  theme_minimal()


dementia_rankings <- drug_rankings %>%
  filter(
    Chemical %in% dementia_chemicals
  )

#DEMENTIA MEDICATION RANKINGS

dementia_rankings

ggplot(
  dementia_rankings,
  aes(
    x = reorder(
      Chemical,
      TotalDisp
    ),
    y = TotalDisp
  )
) +
  geom_col() +
  coord_flip() +
  scale_y_continuous(
    labels = comma
  ) +
  labs(
    title = "National Dispensing of Dementia Pharmaceuticals",
    subtitle = "Donepezil and Rivastigmine",
    x = "Pharmaceutical",
    y = "Number of Dispensings"
  ) +
  theme_minimal()

#PHARMACEUTICAL DISPENSING OVER TIME

year_summary <- chemical_clean %>%
  filter(
    District == "New Zealand"
  ) %>%
  group_by(YearDisp) %>%
  summarise(
    TotalDisp =
      sum(NumDisps),
    
    .groups = "drop"
  )

year_summary

ggplot(
  year_summary,
  aes(
    x = YearDisp,
    y = TotalDisp
  )
) +
  geom_line(
    group = 1,
    linewidth = 1
  ) +
  geom_point(
    size = 3
  ) +
  scale_y_continuous(
    labels = comma,
    limits = c(0, NA)
  ) +
  labs(
    title = "Pharmaceutical Dispensing Over Time",
    x = "Year",
    y = "Number of Dispensings"
  ) +
  theme_minimal()

#DISPENSING BY DISTRICT

district_summary <- chemical_clean %>%
  filter(
    District != "New Zealand",
    District != "Unknown"
  ) %>%
  group_by(District) %>%
  summarise(
    TotalDisp =
      sum(NumDisps),
    
    .groups = "drop"
  ) %>%
  arrange(
    desc(TotalDisp)
  )

district_summary

ggplot(
  district_summary,
  aes(
    x = reorder(
      District,
      TotalDisp
    ),
    y = TotalDisp
  )
) +
  geom_col() +
  coord_flip() +
  scale_y_continuous(
    labels = comma
  ) +
  labs(
    title = "Pharmaceutical Dispensing by District",
    subtitle =
      "Raw dispensing counts are influenced by district population size",
    x = "District",
    y = "Number of Dispensings"
  ) +
  theme_minimal()

#DISPENSING BY DISTRICT

top_five_names <- top_drugs %>%
  slice_head(
    n = 5
  ) %>%
  pull(Chemical)

selected_names <- unique(
  c(
    top_five_names,
    dementia_chemicals
  )
)

selected_names

#SELECTED MEDICATIONS OVER TIME

drug_year <- chemical_clean %>%
  filter(
    District == "New Zealand",
    Chemical %in% selected_names
  ) %>%
  group_by(
    Chemical,
    YearDisp
  ) %>%
  summarise(
    TotalDisp =
      sum(NumDisps),
    
    .groups = "drop"
  )

drug_year

ggplot(
  drug_year,
  aes(
    x = YearDisp,
    y = TotalDisp,
    colour = Chemical,
    group = Chemical
  )
) +
  geom_line(
    linewidth = 1
  ) +
  geom_point(
    size = 2
  ) +
  scale_y_continuous(
    labels = comma,
    limits = c(0, NA)
  ) +
  labs(
    title = "Dispensing Trends of Selected Pharmaceuticals",
    subtitle =
      "Top pharmaceuticals and dementia medications",
    x = "Year",
    y = "Number of Dispensings",
    colour = "Pharmaceutical"
  ) +
  theme_minimal()

#PERCENTAGE CHANGE FROM 2020 TO 2024

drug_change <- drug_year %>%
  filter(
    YearDisp %in% c(
      2020,
      2024
    )
  ) %>%
  select(
    Chemical,
    YearDisp,
    TotalDisp
  ) %>%
  pivot_wider(
    names_from = YearDisp,
    values_from = TotalDisp,
    names_prefix = "Year_"
  ) %>%
  mutate(
    PercentChange =
      (
        Year_2024 -
          Year_2020
      ) /
      Year_2020 *
      100
  ) %>%
  arrange(
    desc(PercentChange)
  )

drug_change

#PEOPLE RECEIVING PHARMACEUTICALS OVER TIME

people_year <- chemical_clean %>%
  filter(
    District == "New Zealand"
  ) %>%
  group_by(YearDisp) %>%
  summarise(
    TotalPeople =
      sum(NumPpl),
    
    .groups = "drop"
  )

people_year

ggplot(
  people_year,
  aes(
    x = YearDisp,
    y = TotalPeople
  )
) +
  geom_line(
    group = 1,
    linewidth = 1
  ) +
  geom_point(
    size = 3
  ) +
  scale_y_continuous(
    labels = comma,
    limits = c(0, NA)
  ) +
  labs(
    title = "Number of People Receiving Pharmaceuticals Over Time",
    x = "Year",
    y = "Number of People"
  ) +
  theme_minimal()

#PEOPLE RECEIVING PHARMACEUTICALS OVER TIME

dispensing_people <- chemical_clean %>%
  filter(
    District == "New Zealand",
    Chemical %in% selected_names,
    NumPpl > 0
  ) %>%
  group_by(
    Chemical,
    YearDisp
  ) %>%
  summarise(
    TotalDisp =
      sum(NumDisps),
    
    TotalPeople =
      sum(NumPpl),
    
    .groups = "drop"
  ) %>%
  mutate(
    DispensingPerPerson =
      TotalDisp /
      TotalPeople
  )

dispensing_people

ggplot(
  dispensing_people,
  aes(
    x = YearDisp,
    y = DispensingPerPerson,
    colour = Chemical,
    group = Chemical
  )
) +
  geom_line(
    linewidth = 1
  ) +
  geom_point() +
  labs(
    title = "Dispensings per Person for Selected Pharmaceuticals",
    x = "Year",
    y = "Dispensings per Person",
    colour = "Pharmaceutical"
  ) +
  theme_minimal()

#DEMENTIA THERAPEUTIC GROUPS

names(lookup)

dementia_groups <- lookup %>%
  filter(
    Chemical %in% dementia_chemicals
  ) %>%
  select(
    Chemical,
    TherapeuticGrp2,
    TherapeuticGrp3
  ) %>%
  distinct()

dementia_groups

#TG2 RANKINGS

tg2_rankings <- tg2_clean %>%
  filter(
    District == "New Zealand"
  ) %>%
  group_by(TherapeuticGrp2) %>%
  summarise(
    TotalDisp =
      sum(NumDisps),
    
    .groups = "drop"
  ) %>%
  arrange(
    desc(TotalDisp)
  ) %>%
  mutate(
    Rank = row_number()
  )

top_tg2 <- tg2_rankings %>%
  slice_head(
    n = 15
  )

top_tg2

ggplot(
  top_tg2,
  aes(
    x = reorder(
      TherapeuticGrp2,
      TotalDisp
    ),
    y = TotalDisp
  )
) +
  geom_col() +
  coord_flip() +
  scale_y_continuous(
    labels = comma
  ) +
  labs(
    title = "Top 15 Therapeutic Groups",
    x = "Therapeutic Group",
    y = "Number of Dispensings"
  ) +
  theme_minimal()

dementia_tg2 <- dementia_groups %>%
  distinct(
    TherapeuticGrp2
  ) %>%
  left_join(
    tg2_rankings,
    by = "TherapeuticGrp2"
  )

dementia_tg2

#TG3 RANKINGS

tg3_rankings <- tg3_clean %>%
  filter(
    District == "New Zealand"
  ) %>%
  group_by(TherapeuticGrp3) %>%
  summarise(
    TotalDisp =
      sum(NumDisps),
    
    .groups = "drop"
  ) %>%
  arrange(
    desc(TotalDisp)
  ) %>%
  mutate(
    Rank = row_number()
  )

top_tg3 <- tg3_rankings %>%
  slice_head(
    n = 15
  )

top_tg3

ggplot(
  top_tg3,
  aes(
    x = reorder(
      TherapeuticGrp3,
      TotalDisp
    ),
    y = TotalDisp
  )
) +
  geom_col() +
  coord_flip() +
  scale_y_continuous(
    labels = comma
  ) +
  labs(
    title = "Top 15 Therapeutic Subgroups",
    x = "Therapeutic Subgroup",
    y = "Number of Dispensings"
  ) +
  theme_minimal()

dementia_tg3 <- dementia_groups %>%
  distinct(
    TherapeuticGrp3
  ) %>%
  left_join(
    tg3_rankings,
    by = "TherapeuticGrp3"
  )

dementia_tg3

