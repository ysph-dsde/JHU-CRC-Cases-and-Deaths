## ----------------------------------------------------------------------------
## From Yale's Public Health Data Science and Data Equity (DSDE) Team
##
## Workshop: Getting Started with Git and GitHub
## Authors:  Shelby Golden, M.S.
## Date:     2024-10-11
## 
## R version:    4.4.1
## renv version: 1.0.11
##
## Description: Worked-through example generating line and bar graphs using the
##              JHU CRC's vaccination data from their GovEX GitHub repo. Refer
##              to the main directory README file for additional information.


## ----------------------------------------------------------------------------
## SET UP THE ENVIRONMENT

## renv() will install all of the packages and their correct version used here.
renv::restore()

## Load in the R packages used in this script from the project library.
library(readr)
library(tidyr)
library(dplyr)
library(stringr)
library(lubridate)
library(scales)
library(ggplot2)


## This function will check if an element is not in a vector.
"%!in%" <- function(x,y)!('%in%'(x,y))




## ----------------------------------------------------------------------------
## LOAD IN THE DATA

## This data is from the COVID-19 Data Repository by the Center for Systems 
## Science and Engineering (CSSE) at Johns Hopkins University (JHU), GitHub 
## GovEX. Additional details can be found in the project repositories main 
## directory's README file.

## We load it in directly from their GitHub page using the raw URL.

df.url <- "https://raw.githubusercontent.com/govex/COVID-19/refs/heads/master/data_tables/vaccine_data/us_data/time_series/time_series_covid19_vaccine_us.csv"
df     <- read_csv(file = df.url, show_col_types = FALSE) %>% as.data.frame()


## The JHU Coronavirus Resource Center (JHU CRC) GovEX repository includes a
## census population file with intercensal estimates and projections from 2010:
## location:      govex/COVID-19/tree/master/data_tables/Data_for_UScounty_map
## file:          PovertyEstimates.xls
## downloaded as: U.S. Department of Agriculture_Population Estimates 2010 to 2018_JHU CRC.xls
##
## This file only covers U.S. population estimates from 2010 to 2018. In the
## GitHub for this project, file "Population Estimates and Projections" describes
## how JHU's original file was harmonized with two other U.S. Census Bureau and
## U.S. Department of Agriculture files containing the same information.


## We load this harmonized file, which spans population intercensal estimations
## and projections from 2010 to 2023 based on the 2010 and 2020 U.S. Census'.

census_2010to2023 <- read.csv(
  "Population Estimates and Projections/US_Census Population Estimates_2010 to 2023.csv",
  header = TRUE) %>% .[, -1]


## View the first and last few rows of the data.
head(df)
tail(df)


## Number of rows and columns.
dim(df)




## ----------------------------------------------------------------------------
## DATA PREPARATION

## First we'll see how the raw data plots. The GovEX README file for the data
## set says that reported vaccinations are cumulative. Therefore, we expect the
## plots to appear monotonic.

df %>%
  filter(Province_State %!in% NA) %>%
  ggplot(data = ., aes(x = Date, y = People_at_least_one_dose)) +
      geom_line(aes(color = Province_State)) +
      scale_y_continuous(labels = unit_format(unit = "M", scale = 1e-6)) +
      scale_x_date(date_breaks = "4 month", date_labels =  "%b %Y") +
      labs(title = "Raw Data Line Plot",
           x = "Daily Updates", y = "People With at Least One Dose") +
      theme_minimal() + theme(legend.position = "none")


## It appears that some state/territories cumulative counts are purely monotonic, 
## but some appear to have some irregularities. These irregularities seem to be
## effecting a limited range of dates, and are not wide-spread nor do they show
## a dramatic departure from what we expect.
## 
## Please refer to the Cleaning Raw Data/Cleaning Script_Vaccinations.R script
## for the complete details about how the raw data from JHU CRC's GovEX repo was
## cleaned and transformed. Here, we will only load the cleaned data produced
## by that script.
## 
## To summarize, the following data cleaning and transforming steps were taken:
##    - Reconcile presence of NA's
##    - Identify and remove duplicate row entries.
##    - Smooth the cumulative counts so they are monotonically increasing over
##      the entire span of dates by fitting with an isotonic regression. NOTE:
##      Column names with a "yf" indicate those values are smoothed.
##    - Back-calculate daily rates from the cumulative sums using the smoothed
##      values, so as to prevent introduction of negative counts.
##    - Aggregate the data to from daily counts to monthly counts.
##    - Basic data set reformatting: reorder the row entries by state names, etc.

df_cleaned <- read_csv("Vaccinations Aggregated by Month.csv", show_col_types = FALSE) %>%
  as.data.frame() %>% .[, -1]


## View the first and last few rows of the data.
head(df_cleaned)
tail(df_cleaned)


## Number of rows and columns.
dim(df_cleaned)


## We can double check that the imported, cleaned data set plots the cumulative
## and daily counts correctly when aggregated to monthly updates.
df_cleaned %>%
  filter(Province_State %!in% "United States") %>%
  ggplot(data = ., aes(x = Month, y = People_at_least_one_dose_yf)) +
      geom_line(aes(color = Province_State)) +
      scale_y_continuous(labels = unit_format(unit = "M", scale = 1e-6)) +
      scale_x_date(date_breaks = "4 month", date_labels =  "%b %Y") +
      labs(title = "Aggregated Data Line Plot\nMax of Cumulative Sum",
           x = "Monthly Updates", y = "People With at Least One Dose `yf`") +
      theme_minimal() + theme(legend.position = "none")



df_cleaned %>%
  filter(Province_State %!in% "United States") %>%
  ggplot(data = ., aes(x = Month, y = People_at_least_one_dose_yf_daily)) +
      geom_line(aes(color = Province_State)) +
      scale_y_continuous(labels = unit_format(unit = "M", scale = 1e-6)) +
      scale_x_date(date_breaks = "4 month", date_labels =  "%b %Y") +
      labs(title = "Aggregated Data Line Plot\nSum of Daily Counts",
           x = "Monthly Updates", y = "People With at Least One Dose `yf`") +
      theme_minimal() + theme(legend.position = "none")


## All seems to be in order so far, however, while we are comparing the raw
## counts between states, it is much more meaningful to compare their
## vaccination rates as a percentage of the population. We can do this
## normalization using the harmonized census population estimates.




## ----------------------------------------------------------------------------
## NORMALIZE TO GET PERCENTAGE OF POPULATION VACCINATED

## First we will inspect the harmonized census population estimates.
## NOTE: Vintages are updated annually based on the current year of the update
## and only spans the dates between the recent census year to the last
## intercensal year preceding the next one. The harmonized data set combines
## vintages from the 2010 to 2019 census estimates and the 2020 to 2023
## census estimates. Additional details can be found in the "Population 
## Estimates and Projections" subdirectory of this projects GitHub repo.

colnames(census_2010to2023)

## There are a number of columns of information that we do not require; we
## only need those reflecting the population estimates for each year.

subset_census <- cbind(census_2010to2023[, c("State")], 
                       # Extract columns with that contain the "Pop_Estimate"
                       # string, with a string detection boundary preceding
                       # the word.
                       census_2010to2023[str_detect(colnames(census_2010to2023), 
                                                    "\\bPop_Estimate")]) %>%
  # Correct the column names.
  `colnames<-`(c("State", colnames(.)[-1]))

dim(subset_census)
colnames(subset_census)


## We see that this successfully extracted the columns we require for our
## normalization. Now we'll check the different states or territories
## that are included in this census data set.

census_2010to2023$State
unique(df_cleaned$Province_State) %>% .[. %!in% census_2010to2023$State]


## Compared with our vaccination counts data set, we will not be able
## to normalize every Province_State, as only the U.S. states and the District 
## of Columbia census data was recorded in the census extrapolation report. 
## We will filter out the regions and population designations that are not 
## represented in the census data.

df_filtered <- filter(df_cleaned, df_cleaned$Province_State %in% subset_census$State)

df_filtered$Province_State %>% unique()


## Now we'll normalize the vaccination daily counts by the population estimates
## for that year to get the percent of the population that was vaccinated.

# Store the column names we are interested in by cumulative and daily counts.
cumulative_counts <- colnames(df_cleaned)[3:6]
daily_counts      <- colnames(df_cleaned)[7:10]

result = list()
for (i in 1:length(subset_census$State)) {
  # Separate out the rows associated with one "Province_State" entry.
  subset <- df_filtered %>% .[.$Province_State %in% subset_census$State[i], ]
  
  # Collect the year that the observation was made.
  staged_dates <- subset[, "Month"] %>% year()
  
  relevant_pop_est = c()
  for (j in 1:length(staged_dates)) {
    # Create a vector with the appropriate intercensal year population estimates
    # that matches the staged_dates vector entry-by-entry.
    relevant_pop_est[j] <- subset_census[i, str_detect(colnames(subset_census), 
                                                       str_c("_", staged_dates[j]))]
  }
  
  # Normalize over all columns for vaccination daily counts.
  precentages <- subset[, daily_counts] %>% sapply(., function(x)
    round((x / relevant_pop_est) * 100, 0))
  
  # Save the results and join them with the metadata columns for merging 
  # back to the main data set.
  result[[i]] <- cbind(subset[, c("Month", "Province_State")], precentages)
}
# Combine the results (list format) into a data frame by adding rows. Each
# row reflects the percentage results for one "Province_State" and each column
# represents the vaccine counting method.
result <- do.call(rbind, result) %>% as.data.frame() %>%
  `colnames<-`(c("Month", "Province_State", str_c(cumulative_counts, "_percent")))

# Merge the previous data set with the normalized daily counts, combining by
# unique matches with the "Month" and "Province_State" columns.
df_total <- merge(df_filtered, result, by = c("Month", "Province_State"))




## ----------------------------------------------------------------------------
## GENERATE AND SAVE OUR PLOT

# Vector with the unique entries for "Province_State", excluding the "United
# States" total counts.
unique_states = unique(df_total$Province_State)[unique(df_total$Province_State) %!in% "United States"]

# Recall the following y-axis variable options:
#     - "_yf"         <- cumulative counts smoothed to be monotonically increasing.
#     - "_yf_daily"   <- back-calculated daily counts.
#     - "_yf_percent" <- daily counts normalized by census data for percentage
#                        of the population that is vaccinated.
colnames(df_total)[-2]



# Line plot:

line_plot <- df_total %>%
  # Filter the data to plot only a selection of "Province_State" entries.
  filter(Province_State != "United States") %>%
  # Generate the plot with time as the x-axis and vaccinations as the y-axis.
  ggplot(data = ., aes(x = Month, y = Doses_admin_yf_percent)) +
      # Generate a line plot and separate data by unique "Province_State".
      geom_line(aes(color = Province_State)) +
      # Format the y-axis to show values as a percent.
      scale_y_continuous(labels = function(x) paste0(x, "%")) +
      # Format the x-axis to show dates as Jan 2020 from 01/01/2020, spaced
      # every four months.
      scale_x_date(date_breaks = "4 month", date_labels =  "%b %Y") +
      # Title, x-axis, and y-axis labels. NOTE: "\n" forms a new line.
      labs(title = "People Fully Vaccinated by State/Territory",
           x = "Month", y = "People Fully Vaccinated") +
      # Graph displays as minimial without a legend.
      theme_minimal() + theme(legend.position = "none")


# Bar plot:

bar_plot <- df_total %>%
  filter(Province_State %in% unique(df_total$Province_State)[c(1, 4, 20)]) %>%
      ggplot(data = ., aes(x = Month, y = Doses_admin_yf_percent)) +
      # stat = "identity" tells the algorithm to not aggregate values, but
      # plot them as provided. alpha = 0.25 fills the bars with 25% opacity.
      geom_bar(stat = "identity", alpha = 0.25,
               aes(color = Province_State, fill = Province_State)) +
      labs(title = "People Fully Vaccinated by State/Territory",
           x = "Month", y = "People Fully Vaccinated") +
      theme_minimal() #+ theme(legend.position = "none")



# Save a plot as a jpeg file.

bar_plot %>% ggsave("plot.jpeg", ., width = 10, height = 6, units = "in", dpi = 300)






