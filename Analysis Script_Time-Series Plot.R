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
library(dplyr)
library(ggplot2)


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

## Check the variable class of each column
sapply(df, class)     # works better than apply() in this scenario


## Check for missing values
sapply(df, function(x) sum(is.na(x))) %>% 
        as.data.frame() %>% `colnames<-`("NA Count")


## Investigate the NA province
na_province  = df[df$Province_State %in% NA, ] 

na_province     %>% head()
na_province$UID %>% unique()

df[df$Province_State %in% NA, "Province_State"] <- "US"


## Aggregate the data by month and state/territory
## We no longer need columns "Date", "UID", or "Country_Region", and
## so we will remove them from the data frame.

df <- df %>% group_by(month = lubridate::floor_date(Date, "month")) %>% 
          as.data.frame()

df_byMonth <- aggregate(. ~ month + Province_State, df[, -c(1:2, 4)], sum)


## There are 50 States, 5 Major Territories, 1 district, and 1 total.
unique_provinces = df_byMonth$Province_State %>% unique()
length(unique_provinces)


## We have an excess of 5 rows, which are not states or territories.
## What are they?
us_state        = datasets::state.name
us_territories  = c("American Samoa", "Guam", "Northern Mariana Islands", 
                    "Puerto Rico", "Virgin Islands")
us_district     = "District of Columbia"


extra_provinces = unique_provinces %>% 
                      .[. %!in% c(us_state, us_territories, us_district, "US")]
extra_provinces


## Check each province has entries for the same number of months
## Boolean test is TRUE when all provinces have the same number of months
table(df_byMonth$Province_State, df_byMonth$month) %>% 
        sapply(., function(x) x == 1) %>% all()




## ----------------------------------------------------------------------------
## TIME-SERIES PLOTS

## US states, major territories, and the district of Columbia
plot_1 <- df_byMonth %>%
            filter(Province_State != "US" & Province_State %!in% extra_provinces) %>%
            ggplot(data = ., aes(x = month, y = People_fully_vaccinated)) +
                  geom_line(aes(color = Province_State)) +
                  scale_y_continuous(labels = scales::comma) +
                  labs(title = "People Fully Vaccinated by State/Territory",
                       x = "Month",
                       y = "People Fully Vaccinated") +
                  theme_minimal()


## Compare two states
plot_2 <- df_byMonth %>%
            filter(Province_State %in% c("Connecticut", "Kansas")) %>%
            ggplot(data = ., aes(x = month, y = People_fully_vaccinated)) +
                  geom_line(aes(color = Province_State), size = 1) +
                  scale_y_continuous(labels = scales::comma) +
                  labs(title = "People Fully Vaccinated\nConnecticut vs. Kansas",
                       x = "Month",
                       y = "People Fully Vaccinated") +
                  theme_minimal()


## Save a plot as a jpeg file
plot_1 %>% ggsave("plot.jpeg", ., width = 10, height = 6, units = "in", dpi = 300)




