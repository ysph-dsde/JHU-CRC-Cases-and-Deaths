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


## ----------------------------------------------------------------------------
## LOAD IN THE DATA
## This data is from the COVID-19 Data Repository by the Center for Systems 
## Science and Engineering (CSSE) at Johns Hopkins University. We load it in
## directly from their GitHub page using the raw URL.

df.url <- "https://raw.githubusercontent.com/govex/COVID-19/refs/heads/master/data_tables/vaccine_data/us_data/time_series/time_series_covid19_vaccine_doses_admin_US.csv"
df     <- read_csv(file = curl(df.url), show_col_types = FALSE) %>%
              as.data.frame()

## View the first and last few rows of the data
head(df)[, 1:15]
tail(df)[, 1:15]


## ----------------------------------------------------------------------------
## DATA PREPARATION

## Number of rows and columns
dim(df)


## There are 55 US States and Major Territories, so what are the remaining 6?




## Check the variable class of each column
apply(df, 2, class) %>% table()

df[, -c(2, 3, 6, 7, 8, 11)] <- apply(df[, -c(2, 3, 6, 7, 8, 11)], 2, function(x) as.numeric(x))


df$Admin2 %>% unique() 










