## ----------------------------------------------------------------------------
## From Yale's Public Health Data Science and Data Equity (DSDE) Team
##
## Workshop: Getting Started with Git and GitHub
## Authors:  Howard Baik, M.S.
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

covid19_confirmed_url <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/refs/heads/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv"
covid19_confirmed_raw  <- read_csv(file = covid19_confirmed_url, show_col_types = FALSE)  

covid19_death_url <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/refs/heads/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_US.csv"
covid19_death_raw  <- read_csv(file = covid19_death_url, show_col_types = FALSE) 

## ----------------------------------------------------------------------------
## DATA PREPARATION

# Prepare data for plotting
covid19_confirmed_processed <- covid19_confirmed_raw %>%
  pivot_longer(cols = "1/22/20":"3/9/23",
               names_to = "date",
               values_to = "cumulative_count") %>%
  mutate(date = mdy(date)) %>% 
  group_by(date) %>% 
  summarise(cumulative_count = sum(cumulative_count, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(daily_count = c(cumulative_count[1], diff(cumulative_count)))



# Prepare data for plotting
covid19_death_processed <- covid19_death_raw %>%
  pivot_longer(cols = "1/22/20":"3/9/23",
               names_to = "date",
               values_to = "cumulative_count") %>%
  mutate(date = mdy(date)) %>% 
  group_by(date) %>% 
  summarise(cumulative_count = sum(cumulative_count, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(daily_count = c(cumulative_count[1], diff(cumulative_count)))
  
  



## ----------------------------------------------------------------------------
## DATA VISUALIZATION

# Plot 1: Cases
plot_cases <- covid19_confirmed_processed %>% 
  # Rename column names so they look nicer in plotly
  rename(Date = date, Count = daily_count) %>% 
  ggplot(aes(Date, Count)) +
  geom_line(color = "#00356b") +
  scale_x_date(date_labels = "%m/%Y",
               breaks = as.Date(c("2020-01-01", "2021-01-01",
                                  "2022-01-01", "2023-01-01"))) +
  scale_y_continuous(labels = scales::label_comma()) +
  labs(x = NULL, 
       y = "Daily Counts", 
       title = "Daily Confirmed Counts of COVID-19 in the US") +
  theme_minimal()

# Make plot interactive
ggplotly(plot_cases)  

# Plot 2: Deaths
plot_deaths <- covid19_death_processed %>% 
  filter(daily_count > 0) %>% 
  # Rename column names so they look nicer in plotly
  rename(Date = date, Count = daily_count) %>% 
  ggplot(aes(Date, Count)) +
  geom_line(color = "#880808") +
  scale_x_date(date_labels = "%m/%Y",
               breaks = as.Date(c("2020-01-01", "2021-01-01",
                                  "2022-01-01", "2023-01-01"))) +
  scale_y_continuous(labels = scales::label_comma()) +
  labs(x = NULL, 
       y = "Daily Counts",
       title = "Daily Death Counts of COVID-19 in the US") +
  theme_minimal()

# Make plot interactive
ggplotly(plot_deaths)
