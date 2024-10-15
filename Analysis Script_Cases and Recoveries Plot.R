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
library(curl)
library(tidyr)


## Function to check if an element is not in a vector
"%!in%" <- function(x,y)!('%in%'(x,y))


## ----------------------------------------------------------------------------
## LOAD IN THE DATA
## This data is from the COVID-19 Data Repository by the Center for Systems 
## Science and Engineering (CSSE) at Johns Hopkins University. We load it in
## directly from their GitHub page using the raw URL.

df.url <- 
df     <- read_csv(file = curl(df.url), show_col_types = FALSE) %>%
              as.data.frame()


## View the first and last few rows of the data
head(df)
tail(df)


## Number of rows and columns
dim(df)


## ----------------------------------------------------------------------------
## DATA PREPARATION









