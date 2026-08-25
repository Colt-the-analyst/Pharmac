library(tidyverse)
library(janitor)
library(scales)

chemical <- read.csv("Data_ByChemical.csv")
chemform <- read.csv("Data_ByChemForm.csv")
tg2 <- read.csv("Data_ByTG2.csv")
tg3 <- read.csv("Data_ByTG3.csv")
lookup <- read.csv("PharmaceuticalsLookup.csv")

dim(chemical)
dim(chemform)
dim(tg2)
dim(tg3)
dim(lookup)

names(chemical)
names(chemform)
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

# Years
table(chemical$YearDisp)

# Districts
table(chemical$District)

# Dispensing types
table(chemical$Type)

# Number of chemicals
n_distinct(chemical$Chemical)

chemical <- chemical %>%
  mutate(
    DispStatus = ifelse(
      NumDisps == "<6",
      "Suppressed (<6)",
      "Exact value"
    ),
    
    PeopleStatus = ifelse(
      NumPpl == "<6",
      "Suppressed (<6)",
      "Exact value"
    )
  )
table(chemical$DispStatus)
table(chemical$PeopleStatus)


chemical %>%
  summarise(
    TotalRows = n(),
    SuppressedDisps = sum(NumDisps == "<6"),
    SuppressedPeople = sum(NumPpl == "<6"),
    PercentDispSuppressed =
      mean(NumDisps == "<6") * 100,
    PercentPeopleSuppressed =
      mean(NumPpl == "<6") * 100
  )

suppression_summary <- chemical %>%
  count(DispStatus)

ggplot(
  suppression_summary,
  aes(x = DispStatus, y = n)
) +
  geom_col() +
  labs(
    title = "Suppression Status of Dispensing Observations",
    x = "Status",
    y = "Number of Observations"
  ) +
  theme_minimal()

suppression_year <- chemical %>%
  group_by(YearDisp) %>%
  summarise(
    Total = n(),
    Suppressed = sum(NumDisps == "<6"),
    SuppressionRate =
      Suppressed / Total * 100
  )

suppression_year
ggplot(
  suppression_year,
  aes(x = YearDisp,
      y = SuppressionRate,
      group = 1)
) +
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
  labs(
    title = "Suppression Rate by Year",
    x = "Year",
    y = "Suppressed observations (%)"
  ) +
  theme_minimal()




suppression_district <- chemical %>%
  filter(District != "New Zealand") %>%
  group_by(District) %>%
  summarise(
    Total = n(),
    Suppressed = sum(NumDisps == "<6"),
    SuppressionRate =
      Suppressed / Total * 100
  ) %>%
  arrange(desc(SuppressionRate))

ggplot(
  suppression_district,
  aes(
    x = reorder(District, SuppressionRate),
    y = SuppressionRate
  )
) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Suppression Rate by District",
    x = "District",
    y = "Suppressed observations (%)"
  ) +
  theme_minimal()

chemical_exact <- chemical %>%
  filter(NumDisps != "<6") %>%
  mutate(
    NumDispsExact = as.numeric(NumDisps)
  )


top_drugs <- chemical_exact %>%
  filter(District == "New Zealand") %>%
  group_by(Chemical) %>%
  summarise(
    TotalDisp = sum(NumDispsExact)
  ) %>%
  arrange(desc(TotalDisp)) %>%
  slice_head(n = 15)

top_drugs

ggplot(
  top_drugs,
  aes(
    x = reorder(Chemical, TotalDisp),
    y = TotalDisp
  )
) +
  geom_col() +
  coord_flip() +
  scale_y_continuous(labels = comma) +
  labs(
    title = "Top 15 Pharmaceuticals by Dispensing",
    x = "Pharmaceutical",
    y = "Number of dispensings"
  ) +
  theme_minimal()

year_summary <- chemical_exact %>%
  filter(District == "New Zealand") %>%
  group_by(YearDisp) %>%
  summarise(
    TotalDisp = sum(NumDispsExact)
  )

year_summary

ggplot(
  year_summary,
  aes(
    x = YearDisp,
    y = TotalDisp,
    group = 1
  )
) +
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
  scale_y_continuous(labels = comma) +
  labs(
    title = "Pharmaceutical Dispensing Over Time",
    x = "Year",
    y = "Number of dispensings"
  ) +
  theme_minimal()

district_summary <- chemical_exact %>%
  filter(District != "New Zealand") %>%
  group_by(District) %>%
  summarise(
    TotalDisp = sum(NumDispsExact)
  ) %>%
  arrange(desc(TotalDisp))
ggplot(
  district_summary,
  aes(
    x = reorder(District, TotalDisp),
    y = TotalDisp
  )
) +
  geom_col() +
  coord_flip() +
  scale_y_continuous(labels = comma) +
  labs(
    title = "Pharmaceutical Dispensing by District",
    x = "District",
    y = "Number of dispensings"
  ) +
  theme_minimal()

top_drugs %>%
  select(Chemical, TotalDisp)

selected_names <- top_drugs %>%
  slice_head(n = 5) %>%
  pull(Chemical)

selected_drugs <- chemical_exact %>%
  filter(Chemical %in% selected_names)

drug_year <- selected_drugs %>%
  filter(District == "New Zealand") %>%
  group_by(Chemical, YearDisp) %>%
  summarise(
    TotalDisp = sum(NumDispsExact),
    .groups = "drop"
  )

ggplot(
  drug_year,
  aes(
    x = YearDisp,
    y = TotalDisp,
    colour = Chemical,
    group = Chemical
  )
) +
  geom_line(linewidth = 1) +
  geom_point() +
  scale_y_continuous(labels = comma) +
  labs(
    title = "Dispensing Trends of Selected Pharmaceuticals",
    x = "Year",
    y = "Number of dispensings",
    colour = "Pharmaceutical"
  ) +
  theme_minimal()

selected_medication <- selected_names[1]
drug_district <- chemical_exact %>%
  filter(
    Chemical == selected_medication,
    District != "New Zealand"
  ) %>%
  group_by(District) %>%
  summarise(
    TotalDisp = sum(NumDispsExact)
  ) %>%
  arrange(desc(TotalDisp))
ggplot(
  drug_district,
  aes(
    x = reorder(District, TotalDisp),
    y = TotalDisp
  )
) +
  geom_col() +
  coord_flip() +
  scale_y_continuous(labels = comma) +
  labs(
    title = paste(
      "Dispensing of",
      selected_medication,
      "by District"
    ),
    x = "District",
    y = "Number of dispensings"
  ) +
  theme_minimal()


people_exact <- chemical %>%
  filter(NumPpl != "<6") %>%
  mutate(
    NumPplExact = as.numeric(NumPpl)
  )

people_year <- people_exact %>%
  filter(District == "New Zealand") %>%
  group_by(YearDisp) %>%
  summarise(
    TotalPeople = sum(NumPplExact)
  )

ggplot(
  people_year,
  aes(
    x = YearDisp,
    y = TotalPeople,
    group = 1
  )
) +
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
  scale_y_continuous(labels = comma) +
  labs(
    title = "Number of People Receiving Pharmaceuticals Over Time",
    x = "Year",
    y = "Number of people"
  ) +
  theme_minimal()

tg2_exact <- tg2 %>%
  filter(NumDisps != "<6") %>%
  mutate(
    NumDispsExact = as.numeric(NumDisps)
  )

top_tg2 <- tg2_exact %>%
  filter(District == "New Zealand") %>%
  group_by(TherapeuticGrp2) %>%
  summarise(
    TotalDisp = sum(NumDispsExact)
  ) %>%
  arrange(desc(TotalDisp)) %>%
  slice_head(n = 15)
ggplot(
  top_tg2,
  aes(
    x = reorder(TherapeuticGrp2, TotalDisp),
    y = TotalDisp
  )
) +
  geom_col() +
  coord_flip() +
  scale_y_continuous(labels = comma) +
  labs(
    title = "Top 15 Therapeutic Groups",
    x = "Therapeutic group",
    y = "Number of dispensings"
  ) +
  theme_minimal()

tg3_exact <- tg3 %>%
  filter(NumDisps != "<6") %>%
  mutate(
    NumDispsExact = as.numeric(NumDisps)
  )
top_tg3 <- tg3_exact %>%
  filter(District == "New Zealand") %>%
  group_by(TherapeuticGrp3) %>%
  summarise(
    TotalDisp = sum(NumDispsExact)
  ) %>%
  arrange(desc(TotalDisp)) %>%
  slice_head(n = 15)
ggplot(
  top_tg3,
  aes(
    x = reorder(TherapeuticGrp3, TotalDisp),
    y = TotalDisp
  )
) +
  geom_col() +
  coord_flip() +
  scale_y_continuous(labels = comma) +
  labs(
    title = "Top 15 Therapeutic Subgroups",
    x = "Therapeutic group",
    y = "Number of dispensings"
  ) +
  theme_minimal()

chemform_exact <- chemform %>%
  filter(NumDisps != "<6") %>%
  mutate(
    NumDispsExact = as.numeric(NumDisps)
  )
top_forms <- chemform_exact %>%
  filter(District == "New Zealand") %>%
  group_by(ChemForm) %>%
  summarise(
    TotalDisp = sum(NumDispsExact)
  ) %>%
  arrange(desc(TotalDisp)) %>%
  slice_head(n = 15)
ggplot(
  top_forms,
  aes(
    x = reorder(ChemForm, TotalDisp),
    y = TotalDisp
  )
) +
  geom_col() +
  coord_flip() +
  scale_y_continuous(labels = comma) +
  labs(
    title = "Top 15 Pharmaceutical Formulations",
    x = "Formulation",
    y = "Number of dispensings"
  ) +
  theme_minimal()

