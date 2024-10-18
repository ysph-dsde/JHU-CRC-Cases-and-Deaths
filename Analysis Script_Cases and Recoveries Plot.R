## ----------------------------------------------------------------------------
## From Yale's Public Health Data Science and Data Equity (DSDE) Team
##
## Workshop: Getting Started with Git and GitHub
## Authors:  Howard Baik, M.S. and Shelby Golden, M.S.
## Date:     2024-10-15
## 
## R version:    4.4.1
## renv version: 1.0.9


## ----------------------------------------------------------------------------
## SET UP THE ENVIRONMENT
## renv() will install all of the packages and their correct version used here

renv::restore()

library(readr)
library(tidyr)
library(dplyr)
library(ggplot2)
library(lubridate)
library(plotly)

## ----------------------------------------------------------------------------
## LOAD IN THE DATA
## This data is from the COVID-19 Data Repository by the Center for Systems 
## Science and Engineering (CSSE) at Johns Hopkins University. We load it in
## directly from their GitHub page using the raw URL.

covid19_confirmed_url <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/refs/heads/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv"
covid19_confirmed_raw  <- read_csv(file = covid19_confirmed_url, show_col_types = FALSE)  

covid19_death_url <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/refs/heads/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv"
covid19_death_raw  <- read_csv(file = covid19_death_url, show_col_types = FALSE) 

## ----------------------------------------------------------------------------
## DATA PREPARATION

# Wrangle dataset
# Ready data for plotting
covid19_confirmed_processed <- covid19_confirmed_raw %>%
  rename(country_region = `Country/Region`) %>% 
  pivot_longer(cols = "1/22/20":"3/9/23",
               names_to = "date",
               values_to = "daily_count") %>%
  mutate(date = mdy(date)) %>% 
  filter(country_region == "US")

# Wrangle dataset
# Ready data for plotting
covid19_death_processed <- covid19_death_raw %>%
  rename(country_region = `Country/Region`) %>% 
  pivot_longer(cols = "1/22/20":"3/9/23",
               names_to = "date",
               values_to = "daily_count") %>%
  mutate(date = mdy(date)) %>% 
  filter(country_region == "US")
  



## ----------------------------------------------------------------------------
## DATA VISUALIZATION

# Plot 1: Cases
covid19_confirmed_processed %>% 
  ggplot(aes(date, daily_count)) +
  geom_line() +
  scale_y_continuous(labels = scales::label_comma(),
                     limits = c(0, 150000000)) +
  labs(x = "Date", y = "Daily Counts", title = "Daily Confirmed Counts of COVID-19 in the US") +
  theme_minimal()
  

# Plot 2: Deaths
covid19_death_processed %>% 
  ggplot(aes(date, daily_count)) +
  geom_line() +
  scale_y_continuous(labels = scales::label_comma()) +
  labs(x = "Date", y = "Daily Counts", title = "Daily Death Counts of COVID-19 in the US") +
  theme_minimal()


