library(tidyverse)
library(ipumsr)
library(srvyr)
library(lubridate)

ddi <- read_ipums_ddi(here::here("data-prep", "cps_00019.xml"))
data <- read_ipums_micro(ddi)

data <- data %>%
  mutate(
    MONTH = as_factor(lbl_clean(MONTH)),
    EMPSTAT = as_factor(lbl_clean(EMPSTAT)),
    LABFORCE = as_factor(lbl_clean(LABFORCE)),
    COVIDTELEW = as_factor(lbl_clean(COVIDTELEW))
  ) %>%
  filter(!(YEAR == 2021 & MONTH == "October"))

cps <- data %>%
  as_survey_design(weights = WTFINL)

data %>%
  count(COVIDTELEW, EMPSTAT) %>%
  print(n = 30)


cps %>%
  filter(EMPSTAT %in% c("At work", "Has job, not at work last week")) %>%
  filter(COVIDTELEW != "NIU") %>%
  mutate(COVIDTELEW = as.character(COVIDTELEW)) %>%
  group_by(YEAR, MONTH) %>%
  summarize(survey_mean(COVIDTELEW == "Yes"))

summarized_data <- cps %>%
  filter(EMPSTAT %in% c("At work", "Has job, not at work last week")) %>%
  filter(COVIDTELEW != "NIU") %>%
  mutate(
    major_occ = case_when(
      OCC < 3600 ~ "Mngmet/profession",
      OCC < 4700 ~ "Services",
      OCC < 6000 ~ "Sales/office",
      TRUE ~ "other"
    )
  ) %>%
  mutate(COVIDTELEW = as.character(COVIDTELEW)) %>%
  group_by(major_occ, YEAR, MONTH) %>%
  summarize(survey_mean(COVIDTELEW == "Yes")) %>%
  ungroup()

summarized_data %>%
  mutate(date = my(paste(MONTH, YEAR))) %>%
  select(date, remote_prop = coef, major_occ) %>%
  write_csv(here::here("data-prep", "remote-work.csv"))



summarized_data %>%
  mutate(date = my(paste(MONTH, YEAR))) %>%
  ggplot(aes(date, coef, color = major_occ)) +
  geom_line() +
  geom_point()


