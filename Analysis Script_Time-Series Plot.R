## ----------------------------------------------------------------------------
## From Yale's Public Health Data Science and Data Equity (DSDE) Team
##
## Workshop: Getting Started with Git and GitHub
## Authors:  Howard Baik, M.S. and Shelby Golden, M.S.
## Date:     2024-10-11
## 
## R version:    4.4.1
## renv version: 1.0.9


## ----------------------------------------------------------------------------
## SET UP THE ENVIRONMENT
## renv() will install all of the packages and their correct version used here

renv::restore()

library(readr)
library(curl)
library(tidyr)


## Function to check if an element is not in a vector
"%!in%" <- function(x,y)!('%in%'(x,y))


## ----------------------------------------------------------------------------
## LOAD IN THE DATA
## This data is from the COVID-19 Data Repository by the Center for Systems 
## Science and Engineering (CSSE) at Johns Hopkins University. We load it in
## directly from their GitHub page using the raw URL.

df.url <- "https://raw.githubusercontent.com/govex/COVID-19/refs/heads/master/data_tables/vaccine_data/us_data/time_series/time_series_covid19_vaccine_us.csv"
df     <- read_csv(file = curl(df.url), show_col_types = FALSE) %>%
              as.data.frame()


## View the first and last few rows of the data
head(df)
tail(df)


## Number of rows and columns
dim(df)


## ----------------------------------------------------------------------------
## DATA PREPARATION

## There are 50 States, 5 Major Territories, and 1 district. 
## What are the remaining 62 entries for?
df$Province_State %>% unique() %>%
      .[. %!in% datasets::state.name]



## Check the variable class of each column
apply(df, 2, class)

df[, -c(2, 3, 6, 7, 8, 11)] <- apply(df[, -c(2, 3, 6, 7, 8, 11)], 2, function(x) as.numeric(x))


df$Admin2 %>% unique()






## Check the vaccination rate is monotonic
all(df == cummax(df$`2023-03-09`))))








