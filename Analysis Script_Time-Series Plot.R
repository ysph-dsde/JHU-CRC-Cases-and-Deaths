## ----------------------------------------------------------------------------
## From Yale's Public Health Data Science and Data Equity (DSDE) Team
##
## Workshop: Getting Started with Git and GitHub
## Authors:  Shelby Golden, M.S.
## Date:     2024-10-11
## 
## R version:    4.4.1
## renv version: 1.0.11


## ----------------------------------------------------------------------------
## SET UP THE ENVIRONMENT
## renv() will install all of the packages and their correct version used here

renv::restore()

library(readr)
library(tidyr)
library(dplyr)
library(scales)
library(ggplot2)


## Function to check if an element is not in a vector
"%!in%" <- function(x,y)!('%in%'(x,y))




## ----------------------------------------------------------------------------
## LOAD IN THE DATA

## This data is from the COVID-19 Data Repository by the Center for Systems 
## Science and Engineering (CSSE) at Johns Hopkins University (JHU), GitHub 
## GovEX. Additional details can be found in the project repositories main 
## directory's README file.

## We load it in directly from their GitHub page using the raw URL.

df.url <- "https://raw.githubusercontent.com/govex/COVID-19/refs/heads/master/data_tables/vaccine_data/us_data/time_series/time_series_covid19_vaccine_us.csv"
df     <- read_csv(file = df.url, show_col_types = FALSE) %>%
              as.data.frame()


## The JHU Coronavirus Resource Center (JHU CRC) GovEX repository includes a
## census population file with intercensal estimates and projections from 2010:
## location:      govex/COVID-19/tree/master/data_tables/Data_for_UScounty_map
## file:          PovertyEstimates.xls
## downloaded as: U.S. Department of Agriculture_Population Estimates 2010 to 2018_JHU CRC.xls

## This file only covers U.S. population estimates from 2010 to 2018. In the
## GitHub for this project, file "Population Estimates and Projections" describes
## how JHU's original file was harmonized with two other U.S. Census Bureau and
## U.S. Department of Agriculture files containing the same information.

## We load this harmonized file, which spans population intercensal estimations
## and projections from 2010 to 2023 based on the 2010 and 2020 U.S. Census'.

census_2010to2023 <- read_csv("Population Estimates and Projections/US_Census Population Estimates_2010 to 2023.csv") %>%
                          as.data.frame()


## View the first and last few rows of the data
head(df)
tail(df)


## Number of rows and columns
dim(df)




## ----------------------------------------------------------------------------
## DATA PREPARATION

## Check the variable class of each column.
## NOTE: sapply() works better than apply() in this scenario
sapply(df, class)


## The variable classes look as we'd expect. No we'll check for missing values.
sapply(df, function(x) sum(is.na(x))) %>% 
        as.data.frame() %>% `colnames<-`("NA Count")


## There are a large number of NAs associated with the "Province_State" column.
na_province  = df[df$Province_State %in% NA, ] 

na_province     %>% head()
na_province$UID %>% unique()

## UID is the unique Identifier for each row entry, and we see that all NAs
## are associated with the same unique row entry. Based on JHU CRC's
## UID_ISO_FIPS_LookUp_Table.csv, we see that this UID corresponds to the US.
## (link: https://github.com/CSSEGISandData/COVID-19/tree/master/csse_covid_19_data)

df[df$Province_State %in% NA, "Province_State"] <- "United States"



## Check each province has entries for the same number of months
## Boolean test is TRUE when all provinces have the same number of 
## time-stamped entries.

table(df$Province_State, df$Date) %>% sapply(., function(x) x == 1) %>% all()


## We see that not all states/territories have the same number of time-stamped
## entries. Vaccination reporting is also not expected to be precise to the
## degree where weekly reported updates are meaningful (within error).
##
## Therefore, we will aggregate the data by month and state/territory. 
## We no longer need columns "Date", "UID", or "Country_Region", 
## and so we will remove them from the data frame as well.

df <- df %>% group_by(month = lubridate::floor_date(Date, "month")) %>% 
          as.data.frame()

df_byMonth <- aggregate(. ~ month + Province_State, df[, -c(1:2, 4)], sum) %>%
                  `colnames<-`(c("Month", colnames(.)[-1]))


## Good sanity check that the rows are aggregated as expected
head(df_byMonth)
dim(df_byMonth)

## Boolean test is TRUE when all provinces have the same number of months
table(df_byMonth$Province_State, df_byMonth$Month) %>% sapply(., function(x) x == 1) %>% all()


## There are 50 States, 5 Major Territories, 1 district, and 1 total metric.
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


## If it becomes relevant, we now have a way to filter out these extra
## provinces or to select for them if we want to compare these populations.
## 
## Now that basic Extract, Transforming, and Loading (ETL) has been completed,
## and we understand the contents of our data set a little better, we are 
## ready to proceed with plotting




## ----------------------------------------------------------------------------
## INITIAL TIME-SERIES PLOT

## US states, major territories, and the district of Columbia
df_byMonth %>%
    filter(Province_State != "US" & Province_State %!in% extra_provinces) %>%
    ggplot(data = ., aes(x = Month, y = People_at_least_one_dose)) +
          geom_line(aes(color = Province_State)) +
          labs(title = "People Fully Vaccinated by State/Territory",
               x = "Month", y = "People Fully Vaccinated")


## 




## ----------------------------------------------------------------------------
## NORMALIZE TO GET PERCENTAGE OF POPULATION VACCINATED

##
## add the percentage of people fully vaccinated
## check the total population from JHU

## change the plot x-axis to be quarterly with Jan 2021 formatting


head(df_byMonth)
census_2010to2023




## US states, major territories, and the district of Columbia
df_byMonth %>%
    filter(Province_State != "US" & Province_State %!in% extra_provinces) %>%
    ggplot(data = ., aes(x = Month, y = People_at_least_one_dose)) +
           geom_line(aes(color = Province_State)) +
           scale_y_continuous(labels = unit_format(unit = "M", scale = 1e-6)) +
           labs(title = "People Fully Vaccinated by State/Territory",
                x = "Month", y = "People Fully Vaccinated") +
           theme_minimal()



## Compare two states
plot_2 <- df_byMonth %>%
  filter(Province_State %in% c("Connecticut", "Kansas")) %>%
  ggplot(data = ., aes(x = month, y = People_fully_vaccinated)) +
         geom_line(aes(color = Province_State)) +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "People Fully Vaccinated\nConnecticut vs. Kansas",
       x = "Month",
       y = "People Fully Vaccinated") +
  theme_minimal()


## Save a plot as a jpeg file
plot_1 %>% ggsave("plot.jpeg", ., width = 10, height = 6, units = "in", dpi = 300)






