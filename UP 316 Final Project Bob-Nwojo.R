install.packages("tidycensus")
install.packages("tidyverse")
library(tidyverse)
library(tidycensus)
library(dplyr)
library(ggplot2)
library(tidyr)
library(scales)

MN_2015 <- read.csv("MN.B25003-2015.csv")
MN_2018 <- read.csv("MN.B25003-2018.csv") 
MN_2020 <- read.csv("MN.B25003-2020.csv")
MN_2022 <- read.csv("MN.B25003-2022.csv")

HHP_2015<- read.csv("HHP2015.csv")
HHP_2018<- read.csv("HHP2018.csv")
HHP_2020<- read.csv("HHP2020.csv")
HHP_2022<- read.csv("HHP2022.csv")

INC_2015<- read.csv("INC 2015.csv")
INC_2018<- read.csv("INC 2018.csv")
INC_2020<- read.csv("INC 2020.csv")
INC_2022<- read.csv("INC 2022.csv")

HTYP_2015 <- read.csv("HHTYP_2015.csv")
HTYP_2022 <- read.csv("HHTY_2022.csv")


MN_2015 <- MN_2015 %>%
  mutate(Total...Owner.occupied= as.numeric(gsub(",","", Total...Owner.occupied)),
         Total...Renter.occupied= as.numeric(gsub(",","", Total...Renter.occupied)))

MN_2015_total <- MN_2015 %>% 
  summarise(total_owner_occupied = sum(Total...Owner.occupied, na.rm = TRUE),
            total_renter_occupied = sum(Total...Renter.occupied, na.rm = TRUE)) 

MN_2018 <- MN_2018 %>%
  mutate(Total...Owner.occupied= as.numeric(gsub(",","", Total...Owner.occupied)),
         Total...Renter.occupied= as.numeric(gsub(",","", Total...Renter.occupied)))

MN_2018_total <- MN_2018 %>% 
  summarise(total_owner_occupied = sum(Total...Owner.occupied, na.rm = TRUE),
            total_renter_occupied = sum(Total...Renter.occupied, na.rm = TRUE)) 

MN_2020 <- MN_2020 %>%
  mutate(Total...Owner.occupied= as.numeric(gsub(",","", Total...Owner.occupied)),
         Total...Renter.occupied= as.numeric(gsub(",","", Total...Renter.occupied)))

MN_2020_total <- MN_2020 %>% 
  summarise(total_owner_occupied = sum(Total...Owner.occupied, na.rm = TRUE),
            total_renter_occupied = sum(Total...Renter.occupied, na.rm = TRUE)) 

MN_2022 <- MN_2022 %>%
  mutate(Total...Owner.occupied= as.numeric(gsub(",","", Total...Owner.occupied)),
         Total...Renter.occupied= as.numeric(gsub(",","", Total...Renter.occupied)))

MN_2022_total <- MN_2022 %>% 
  summarise(total_owner_occupied = sum(Total...Owner.occupied, na.rm = TRUE),
            total_renter_occupied = sum(Total...Renter.occupied, na.rm = TRUE)) 

colnames(HHP_2015)
colnames(INC_2015)

MN_2015_total <- MN_2015 %>%
  summarise(total_owner= sum(Total...Owner.occupied, na.rm=TRUE),
            total_renter= sum(Total...Renter.occupied, na.rm = TRUE)
            ) %>% mutate (homeownership_rate= total_owner/(total_owner+total_renter))
print(MN_2015_total)

MN_2018_total <- MN_2018 %>%
  summarise(total_owner= sum(Total...Owner.occupied, na.rm=TRUE),
            total_renter= sum(Total...Renter.occupied, na.rm = TRUE)
  ) %>% mutate (homeownership_rate= total_owner/(total_owner+total_renter))
print(MN_2018_total)

MN_2020_total <- MN_2020 %>%
  summarise(total_owner= sum(Total...Owner.occupied, na.rm=TRUE),
            total_renter= sum(Total...Renter.occupied, na.rm = TRUE)
  ) %>% mutate (homeownership_rate= total_owner/(total_owner+total_renter))
print(MN_2020_total)

MN_2022_total <- MN_2022 %>%
  summarise(total_owner= sum(Total...Owner.occupied, na.rm=TRUE),
            total_renter= sum(Total...Renter.occupied, na.rm = TRUE)
  ) %>% mutate (homeownership_rate= total_owner/(total_owner+total_renter))
print(MN_2022_total)

HHP_2015 <- HHP_2015 %>%
  mutate(Median.value..dollars.= as.numeric(gsub(",","", Median.value..dollars.)))
HHP_2018 <- HHP_2018 %>%
  mutate(Median.value..dollars.= as.numeric(gsub(",","", Median.value..dollars.)))
HHP_2020 <- HHP_2020 %>%
  mutate(Median.value..dollars.= as.numeric(gsub(",","", Median.value..dollars.)))
HHP_2022 <- HHP_2022 %>%
  mutate(Median.value..dollars.= as.numeric(gsub(",","", Median.value..dollars.)))

INC_2015 <- INC_2015 %>%
  mutate(Median.household.income.in.the.past.12.months..in.2015.Inflation.adjusted.dollars.= 
           as.numeric(gsub(",","", Median.household.income.in.the.past.12.months..in.2015.Inflation.adjusted.dollars.)))
INC_2018 <- INC_2018 %>%
  mutate(Median.household.income.in.the.past.12.months..in.2018.Inflation.adjusted.dollars.= 
           as.numeric(gsub(",","", Median.household.income.in.the.past.12.months..in.2018.inflation.adjusted.dollars.)))
INC_2020 <- INC_2020 %>%
  mutate(Median.household.income.in.the.past.12.months..in.2020.Inflation.adjusted.dollars.= 
           as.numeric(gsub(",","", Median.household.income.in.the.past.12.months..in.2020.inflation.adjusted.dollars.)))
INC_2022 <- INC_2022 %>%
  mutate(Median.household.income.in.the.past.12.months..in.2022.Inflation.adjusted.dollars.= 
           as.numeric(gsub(",","", Median.household.income.in.the.past.12.months..in.2022.inflation.adjusted.dollars.)))

avg_home_2015 <- HHP_2015 %>%
  summarise(avg_home_value = mean(Median.value..dollars., na.rm = TRUE)) %>%
  mutate(year = 2015)
avg_home_2018 <- HHP_2018 %>%
  summarise(avg_home_value = mean(Median.value..dollars., na.rm = TRUE)) %>%
  mutate(year = 2018)
avg_home_2020 <- HHP_2020 %>%
  summarise(avg_home_value = mean(Median.value..dollars., na.rm = TRUE)) %>%
  mutate(year = 2020)
avg_home_2022 <- HHP_2022 %>%
  summarise(avg_home_value = mean(Median.value..dollars., na.rm = TRUE)) %>%
  mutate(year = 2022)

avg_income_2015 <- INC_2015 %>%
  summarise(avg_median_income = mean(Median.household.income.in.the.past.12.months..in.2015.Inflation.adjusted.dollars., na.rm = TRUE)) %>%
  mutate(year = 2015)
avg_income_2018 <- INC_2018 %>%
  summarise(avg_median_income= mean(Median.household.income.in.the.past.12.months..in.2018.Inflation.adjusted.dollars., na.rm = TRUE)) %>%
  mutate(year = 2018) 
avg_income_2020 <- INC_2020 %>%
  summarise(avg_median_income= mean(Median.household.income.in.the.past.12.months..in.2020.Inflation.adjusted.dollars., na.rm = TRUE)) %>%
  mutate(year = 2020)
avg_income_2022 <- INC_2022 %>%
  summarise(avg_median_income= mean(Median.household.income.in.the.past.12.months..in.2022.Inflation.adjusted.dollars., na.rm = TRUE)) %>%
  mutate(year = 2022)

HTYP_2015 <- HTYP_2015 %>%
  mutate( Total...1..detached = as.numeric(gsub("[^0-9]", "", Total...1..detached)))
HTYP_2022 <- HTYP_2022 %>%
  mutate( Total...1..detached = as.numeric(gsub("[^0-9]", "", Total...1..detached)))
HTYP_2015 <- HTYP_2015 %>%
  mutate(across(
    c(detached, attached, duplex, triplex_4plex, small_apts, medium_apts, large_apts, mobile_home),
    ~ as.numeric(gsub("[^0-9]", "", .))
  ))
HTYP_2022 <- HTYP_2022 %>%
  mutate(across(
    c(detached, attached, duplex, triplex_4plex, small_apts, medium_apts, large_apts, mobile_home),
    ~ as.numeric(gsub("[^0-9]", "", .))
  ))


HTYP_2015 <- HTYP_2015 %>%
  rename(
    detached = Total...1..detached,
    attached = Total...1..attached,
    duplex = Total...2,
    triplex_4plex = Total...3.or.4,
    small_apts = Total...5.to.9,
    medium_apts = Total...10.to.19,
    large_apts = Total...20.to.49,
    mobile_home = Total...Mobile.home
  )
HTYP_2022 <- HTYP_2022 %>%
  rename(
    detached = Total...1..detached,
    attached = Total...1..attached,
    duplex = Total...2,
    triplex_4plex = Total...3.or.4,
    small_apts = Total...5.to.9,
    medium_apts = Total...10.to.19,
    large_apts = Total...20.to.49,
    mobile_home = Total...Mobile.home
  )

MN_2015_total$year <- 2015
MN_2018_total$year <- 2018
MN_2020_total$year <- 2020
MN_2022_total$year <- 2022

MN_combined$year <- as.numeric(MN_combined$year)

MN_combined <- bind_rows(
  MN_2015_total, MN_2018_total, MN_2020_total, MN_2022_total)
print(MN_combined)

home_summary <- bind_rows(avg_home_2015, avg_home_2018, avg_home_2020, avg_home_2022)
income_summary <- bind_rows(avg_income_2015, avg_income_2018, avg_income_2020, avg_income_2022)
afford_summary <- left_join(home_summary, income_summary, by = "year")

HTYPE_2015_TT <- HTYP_2015 %>%
  summarise(
    detached = sum(detached, na.rm = TRUE),
    attached = sum(attached, na.rm = TRUE),
    duplex = sum(duplex, na.rm = TRUE),
    triplex_4plex = sum(triplex_4plex, na.rm = TRUE),
    small_apts = sum(small_apts, na.rm = TRUE),
    medium_apts = sum(medium_apts, na.rm = TRUE),
    large_apts = sum(large_apts, na.rm = TRUE),
    mobile_home = sum(mobile_home, na.rm = TRUE)
  ) %>%
  pivot_longer(cols = everything(), names_to = "type", values_to = "units") %>%
  mutate(year = 2015)

HTYPE_2022_TT <- HTYP_2022 %>%
  summarise(
    detached = sum(detached, na.rm = TRUE),
    attached = sum(attached, na.rm = TRUE),
    duplex = sum(duplex, na.rm = TRUE),
    triplex_4plex = sum(triplex_4plex, na.rm = TRUE),
    small_apts = sum(small_apts, na.rm = TRUE),
    medium_apts = sum(medium_apts, na.rm = TRUE),
    large_apts = sum(large_apts, na.rm = TRUE),
    mobile_home = sum(mobile_home, na.rm = TRUE)
  ) %>%
  pivot_longer(cols = everything(), names_to = "type", values_to = "units") %>%
  mutate(year = 2022)

hS_cOMP <- bind_rows(HTYPE_2015_TT, HTYPE_2022_TT)
head(hS_cOMP)
hS_cOMP <- hS_cOMP %>%
  mutate(type = recode(type,
                       detached = "Single-Family Detached",
                       attached = "Single-Family Attached",
                       duplex = "Duplex (2 Units)",
                       triplex_4plex = "3–4 Units",
                       small_apts = "5–9 Units",
                       medium_apts = "10–19 Units",
                       large_apts = "20+ Units",
                       mobile_home = "Mobile Homes"
  ))


ggplot(hS_cOMP, aes(x = type, y = units, fill = factor(year))) +
  geom_col(position = "dodge") +
  coord_flip() +
  scale_y_continuous(labels = comma) + 
  labs(
    title = "Housing Units by Type in Hennepin County (2015 vs 2022)",
    x = "Housing Type",
    y = "Total Units",
    fill = "Year"
  ) +
  theme_minimal()



ggplot(MN_combined, aes(x = year, y = homeownership_rate)) +
  geom_line(color = "blue") +
  geom_point(size = 3) +
  geom_vline(xintercept = 2019, linetype = "dashed", color = "red") +
  annotate("text", x = 2019, y = 0.63, label = "Policy Enacted", color = "red", angle = 90, vjust = -0.5) +
  scale_y_continuous(
    limits = c(0.60, 0.65),
    breaks = seq(0.60, 0.625, 0.65),
    labels = scales::percent_format(accuracy = 1)
  ) +
  labs(
    title = "Homeownership Rate in Hennepin
    County (2015–2022)",
    x = "Year",
    y = "Homeownership Rate"
  ) +
  theme_minimal()

afford_long <- afford_summary %>%
  pivot_longer(cols = c(avg_home_value, avg_median_income),
               names_to = "type",
               values_to = "value")

afford_long <- afford_long %>%
  mutate(type = recode(type,
                       avg_home_value = "Median Home Value",
                       avg_median_income = "Median Household Income"))

ggplot(afford_long, aes(x = factor(year), y = value, fill = type)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_y_continuous(labels = scales::dollar_format()) +
  labs(
    title = "Housing Affordability (Hennepin County)",
    x = "Year",
    y = "Dollars",
    fill = "Metric"
  ) +
  theme_minimal()

write.csv(Homeownership_summary, "")
