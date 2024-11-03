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
  # Reshape data from wide to long format, with dates as a single column
  pivot_longer(cols = "1/22/20":"3/9/23",
               names_to = "date",
               values_to = "cumulative_count") %>%
  # Convert date column to Date type in month-day-year format
  mutate(date = mdy(date)) %>% 
  # Group data by date
  group_by(date) %>% 
  # Sum counts by date, removing NA values
  summarise(cumulative_count = sum(cumulative_count, na.rm = TRUE)) %>% 
  # Remove grouping
  ungroup() %>% 
  # Calculate daily counts by finding the difference in cumulative counts
  mutate(daily_count = c(cumulative_count[1], diff(cumulative_count)))



# Prepare data for plotting
covid19_death_processed <- covid19_death_raw %>%
  # Reshape data from wide to long format, with dates as a single column
  pivot_longer(cols = "1/22/20":"3/9/23",
               names_to = "date",
               values_to = "cumulative_count") %>%
  # Convert date column to Date type in month-day-year format
  mutate(date = mdy(date)) %>% 
  # Group data by date
  group_by(date) %>% 
  # Sum counts by date, removing NA values
  summarise(cumulative_count = sum(cumulative_count, na.rm = TRUE)) %>% 
  # Remove grouping
  ungroup() %>% 
  # Calculate daily counts by finding the difference in cumulative counts
  mutate(daily_count = c(cumulative_count[1], diff(cumulative_count)))
  
  



## ----------------------------------------------------------------------------
## DATA VISUALIZATION

# Plot 1: Cases
plot_cases <- covid19_confirmed_processed %>% 
  # Rename column names so they look nicer on plot
  rename(Date = date, Count = daily_count) %>% 
  # Start ggplot with date on x-axis and daily count on y-axis
  ggplot(aes(Date, Count)) +
  # Add a line plot with a specified color
  geom_line(color = "#00356b") +
  # Format x-axis dates as month/year
  scale_x_date(date_labels = "%m/%Y",
               breaks = as.Date(c("2020-01-01", "2021-01-01",
                                  "2022-01-01", "2023-01-01"))) +
  # Format the y-axis to display counts with commas
  scale_y_continuous(labels = scales::label_comma()) +
  # Add labels and title to the plot
  labs(x = NULL, 
       y = "Daily Counts", 
       title = "Daily Confirmed Counts of COVID-19 in the US") +
  # Apply a minimal theme to the plot for a clean appearance
  theme_minimal()

# Make plot interactive
ggplotly(plot_cases)  

# Plot 2: Deaths
plot_deaths <- covid19_death_processed %>% 
  filter(daily_count > 0) %>% 
  # Rename column names so they look nicer on plot
  rename(Date = date, Count = daily_count) %>% 
  # Start ggplot with date on x-axis and daily count on y-axis
  ggplot(aes(Date, Count)) +
  # Add a line plot with a specified color
  geom_line(color = "#880808") +
  # Format x-axis dates as month/year
  scale_x_date(date_labels = "%m/%Y",
               breaks = as.Date(c("2020-01-01", "2021-01-01",
                                  "2022-01-01", "2023-01-01"))) +
  # Format the y-axis to display counts with commas
  scale_y_continuous(labels = scales::label_comma()) +
  # Add labels and title to the plot
  labs(x = NULL, 
       y = "Daily Counts",
       title = "Daily Death Counts of COVID-19 in the US") +
  # Apply a minimal theme to the plot for a clean appearance
  theme_minimal()

# Make plot interactive
ggplotly(plot_deaths)
