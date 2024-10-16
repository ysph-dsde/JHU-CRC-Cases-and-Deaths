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


## Function to check if an element is not in a vector
"%!in%" <- function(x,y)!('%in%'(x,y))


## ----------------------------------------------------------------------------
## LOAD IN THE DATA
## This data is from the COVID-19 Data Repository by the Center for Systems 
## Science and Engineering (CSSE) at Johns Hopkins University. We load it in
## directly from their GitHub page using the raw URL.

df.url <- "https://raw.githubusercontent.com/umich-cphds/cov-ind-19-data/refs/heads/master/source_data_latest.csv"
df     <- read_csv(file = df.url, show_col_types = FALSE) 


## View the first and last few rows of the data
head(df)
tail(df)


## Number of rows and columns
dim(df)


## ----------------------------------------------------------------------------
## DATA PREPARATION

# Explore dataset
df %>% 
  select(State) %>% 
  unique()
# 38 unique states in India

# Check for NA values
df %>%
  select(where(~ any(is.na(.)))) %>%
  summarise(across(everything(), ~ sum(is.na(.))))
# 2 NA values in `State`. 68 NA values in `Active`

df_processed <- df %>% 
  # Filter out NA values
  filter(!is.na(State), !is.na(Active)) %>% 
  # Make `Date` column a Date class
  mutate(Date = dmy(Date)) %>% 
  pivot_longer(cols = Cases:Deaths, 
               names_to = "type", 
               values_to = "daily_counts")




## ----------------------------------------------------------------------------
## DATA VISUALIZATION

# Plot 1: Cases
df_processed %>% 
  filter(type %in% c("Cases", "Recovered")) %>% 
  filter(State == "Andhra Pradesh") %>% 
  ggplot(aes(Date, daily_counts)) +
  geom_area(aes(fill = type)) +
  scale_y_continuous(labels = scales::label_comma()) +
  labs(title = "Daily number of new COVID-19 Cases in Andhra Pradesh, India",
       x = "Date",
       y = "Daily Counts",
       fill = NULL) +
  theme_minimal()

# Plot 2: Deaths
df_processed %>% 
  filter(type == "Deaths") %>% 
  # TODO: Filter for one state
  filter(State == "Andhra Pradesh") %>% 
  ggplot(aes(Date, daily_counts)) +
  geom_area(aes(fill = type)) +
  scale_fill_discrete(guide="none") +
  scale_y_continuous(labels = scales::label_comma()) +
  labs(title = "Daily number of new COVID-19 Deaths in Andhra Pradesh, India",
       x = "Date",
       y = "Daily Counts") +
  theme_minimal()


