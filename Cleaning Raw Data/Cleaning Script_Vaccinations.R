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
## Description: This script imports the Johns Hopkins University Coronavirus
##              Resource Center (JHU CRC) vaccination data for U.S. states
##              and territories and pre-processes it for time-series plotting.
##              The data was assessed for irregularities and errors (i.e.
##              duplicated entries and NA's). The cumulative counts were
##              smoothed to be fully monotonically increasing before calculating
##              the daily vaccination rates.

## ----------------------------------------------------------------------------
## SET UP THE ENVIRONMENT

## renv() will install all of the packages and their correct version used here.
renv::restore()

## Load in the R packages used in this script from the project library.
library(readr)
library(tidyr)
library(dplyr)
library(stringr)
library(stats)
library(scales)
library(ggplot2)


## This function will check if an element is not in a vector.
"%!in%" <- function(x,y)!('%in%'(x,y))


## This function confirms that row entries satisfying a filter are the same.
check_matched_row_entries <- function(data, variable, query_filter){
  # Filter the data set by the variable outcome (i.e. "Province_State" == 
  # "United States"). Iterate Boolean test for the number of equal entries
  # over each column in the subset.
  check = data %>% .[.[, variable] == query_filter, ] %>% 
    apply(., 2, function(x) length(unique(x)) == 1) %>% all()
  
  # If statement to interpret the outcome of the Boolean test.
  if(check == TRUE){
    print("All entries are the same.")
  }else{
    print("Some entries are not the same.")
    
  }
}




## ----------------------------------------------------------------------------
## LOAD IN THE DATA

## This data is from the COVID-19 Data Repository managed by the Center for 
## Systems Science and Engineering (CSSE) at Johns Hopkins University (JHU), 
## GitHub GovEX. Additional details can be found in the project GitHub repo's 
## main directory README file.

## We load it in directly from the JHU CRC's GitHub page using the raw URL.
df.url <- "https://raw.githubusercontent.com/govex/COVID-19/refs/heads/master/data_tables/vaccine_data/us_data/time_series/time_series_covid19_vaccine_us.csv"
df     <- read_csv(file = df.url, show_col_types = FALSE) %>%
              as.data.frame()

## View the first and last few rows of the data
head(df)
tail(df)

## Number of rows and columns
dim(df)




## ----------------------------------------------------------------------------
## DATA PREPARATION

## Check the variable class of each column.
## NOTE: sapply() works better than apply() in this scenario.
sapply(df, class) %>% as.data.frame()


## The variable classes look as we'd expect. Now we'll check for missing values.
sapply(df, function(x) sum(is.na(x))) %>%
  as.data.frame() %>% `colnames<-`("NA Count")


## There are a large number of NA's associated with the "Province_State" variable.
na_province  = df[df$Province_State %in% NA, ] 

na_province     %>% head()
na_province$UID %>% unique()


## UID is the Unique Identifier for each row entry, and we see that all NA's
## are associated with the same unique row entry. Based on JHU CRC's
## UID_ISO_FIPS_LookUp_Table.csv, we see that this UID corresponds to the U.S.
## total counts.
## Source: https://github.com/CSSEGISandData/COVID-19/tree/master/csse_covid_19_data

df[df$Province_State %in% NA, "Province_State"] <- "United States"


## There are 50 states, 5 major territories, 1 district, and 1 total metric.
unique_provinces = df$Province_State %>% unique()
length(unique_provinces)


## We have an excess of 5 rows, which are not states or territories.
## What are they?
us_states       = c(datasets::state.name, "District of Columbia") %>% sort()
us_territories  = c("American Samoa", "Guam", "Northern Mariana Islands", 
                    "Puerto Rico", "Virgin Islands")

extra_provinces = unique_provinces %>% .[. %!in% c("United States", us_states, us_territories)]
extra_provinces
## NOTE: Some of these extra types of provinces will double count individuals
## that are also represented in the state/territory-level classification.


## Now we'll see how the data plots. The GovEX README file for the data set
## says that reported vaccinations are cumulative. Therefore, we expect the
## plots to appear monotonic.
df %>%
  filter(Province_State %in% us_states) %>%
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
## We can double check the accuracy of our qualitative assessment with the 
## following loop. This will assess if the data for each "Province_State" (row)
## and vaccine counting type (columns) pairs are monotonically increasing over
## the whole range of dates. TRUE means that it is and FALSE means that there is
## at least one sequence of dates that is not monotonically increasing.

# Store the column names that we will be processed for smoothing.
make_daily_counts <- colnames(df[, -c(1:4)])

monotonic_result <- list()
for (i in 1:length(make_daily_counts)) {
  check <- list()
  for (j in 1:length(unique(df$Province_State))) {
    # Separate out the vector associated with one "Province_State" entry
    # and one vaccination count method (column).
    subset <- df[df$Province_State %in% unique(df$Province_State)[j], make_daily_counts[i]]
    
    # A monotonically increasing range of dates will always have positive
    # successive differences, including zero to reflect no change.
    check[[j]] <- all(diff(subset) >= 0)
  }
  
  # Combine the results (list format) into one data frame by adding rows.
  # Each row reflects the results for one "Province_State".
  monotonic_result[[i]] <- do.call(rbind, check) %>% `rownames<-`(unique(df$Province_State))
}
# Combine the monotonic result (list format) into one data frame by adding
# columns. Each column reflects the results for one vaccination count method.
monotonic_result <- do.call(cbind, monotonic_result) %>%
  `colnames<-`(make_daily_counts) %>% as.data.frame()

monotonic_result


# Table how many states/territories are not monotonically increasing by the 
# vaccination count method out of a total of 62 "Province_State" variables.
sapply(monotonic_result, function(x)
  length(x) - sum(x, na.rm = TRUE)) %>% as.data.frame() %>%
  `colnames<-`("State/Territories Not Monotonic")

# Table how many states/territories have monotonically increasing vaccine counts
# for the full range of dates reported across all four vaccine count methods.
apply(monotonic_result, 1, function(x) all(x)) %>% table()


## Unlike what we would expect, not all of the entries are monotonically
## increasing. Only 11 "Province_State" entries are for all four 
## vaccine count methods.
## 
## One source of this problem could be duplicate entries. We start checking for
## duplicates or other obvious data entry errors.

result <- list()
for (i in 1:length(unique(df$Province_State))) {
  # Separate out the dates vector associated with one "Province_State" entry.
  subset      <- df[df$Province_State %in% unique(df$Province_State)[i], "Date"]
  
  # Store the date ranges that occur more than once.
  result[[i]] <- table(subset)[table(subset) %!in% 1]
}
# Convert the results into a vector and report only the unique values.
lapply(result, names) %>% do.call(c, .) %>% unique()


## Looks like the only entry that was duplicated is associated with 2022-06-17.
## We will check the entries are different.

# Filter out the dates so only the duplicate entries are shown and excess
# metadata columns are removed.
subset <- df[df$Date == "2022-06-17", -c(2, 4)]
for (i in 1:length(unique(df$Province_State))) {
  # Iterate the row-uniqueness test over each possible "Province_State"
  check_matched_row_entries(subset, "Province_State", unique(df$Province_State)[i])
}


## Duplicate entries report the same information. We will remove these by
## arbitrarily selecting one of the duplicates for retention.
df <- df[!duplicated(df), ]

df %>%
  filter(Province_State %in% us_states) %>%
  ggplot(data = ., aes(x = Date, y = People_at_least_one_dose)) +
      geom_line(aes(color = Province_State)) +
      scale_y_continuous(labels = unit_format(unit = "M", scale = 1e-6)) +
      scale_x_date(date_breaks = "4 month", date_labels =  "%b %Y") +
      labs(title = "Raw Data Line Plot - Duplicate Dates Removed",
           x = "Daily Updates", y = "People With at Least One Dose") +
      theme_minimal() + theme(legend.position = "none")


## Removing duplicated dates did not seem to help reduce the observed
## irregularities. Now we'll confirm that the dates are evenly spaced, with
## entries added each day.
## 
## The following test will check for the number of days between successive
## dates and test the value against zero days, one day, or any length not
## zero or one. Results are aggregated for the entire span of dates by
## "Province_State" entries. A passed test will only show TRUE under the
## "Equal 1" column and FALSE for the other two.

result <- list()
for (i in 1:length(unique(df$Province_State))) {
  # Separate out the dates vector associated with one "Province_State" entry.
  subset <- df[df$Province_State %in% unique(df$Province_State)[i], "Date"]
  
  check <- c()
  for (j in 1:(length(subset) - 1)) {
    # Count the number of days between each successive date.
    check[j]  <- difftime(subset[j + 1], subset[j], units = "days") %>% as.numeric()
  }
  
  # Store results as a Boolean. Test will report TRUE if at least one span
  # between dates satisfies the conditional test.
  result[[i]] <- data.frame(
    "Equal 0" = 0 %in% unique(check),
    "Equal 1" = 1 %in% unique(check),
    "Else"    = any(unique(check) > 1 | unique(check) < 0)
  )
}
# Combine the results (list format) into a data frame by adding rows. Each 
# row reflects the results for one "Province_State" span of dates.
result <- do.call(rbind, result) %>% `colnames<-`(c("Equal 0", "Equal 1", "Else")) %>%
  `rownames<-`(unique(df$Province_State))

result


## Looks like they are all spaced by one day, which is what we are looking for.
## The problem seems to be a deeper issue than obvious data-entry errors that
## can be reconciled with the information we have on hand. In the GovEX GitHub, 
## it says the a states vaccination reports were compared with the CDC's Vaccine 
## Tracker report. If they differed, the larger of the two values was saved.
## It is possible that on a few occasions there was an error when recording the 
## largest of the two reported values or that some daily updates were corrected
## to show less vaccinations from a previous date.
##
## To compensate for these blips, we will use an isotonic (monotonically 
## increasing) least squares regression to smooth the data. The function
## that will be used is isoreg() from the stats package. 


## Recall the results from above.
all_col_monotonic <- apply(monotonic_result, 1, function(x)
  all(x)) %>% as.data.frame() %>% `colnames<-`("All Monotonic")

monotonic_result
all_col_monotonic


## First we'll check to confirm the algorithm does a decently good job
## predicting entries we know are monotonically increasing for all vaccine
## counting methods. Outcomes equal to TRUE imply that the predictions are 
## exactly equal with the reported value.

# Extract which "Province_State" entries were monotonically increasing
# for all vaccine counting methods.
test_ir <- row.names(all_col_monotonic)[all_col_monotonic$`All Monotonic` == TRUE]

result = list()
for (i in 1:length(test_ir)) {
  # Subset the data set for one "Province_State" entry, restricting the
  # search space to only those that were monotonically increasing.
  subset <- df[df$Province_State %in% test_ir[i], ]
  
  check_counts = c()
  for (j in 1:length(make_daily_counts)) {
    # Filter out the vaccine count method vector and fit using isoreg().
    ir <- isoreg(subset[, make_daily_counts[j]], y = NULL)
    
    # Check the difference between the reported value and the "yf" prediction.
    # This is an all-or-nothing Boolean test. NOTE: In "yf" "f" means "fitted" 
    # and "y" denote the prediction based on the isotonic fitting results.
    check_counts[j] <- all(subset[, make_daily_counts[j]] - ir$yf == 0)
  }
  result[[i]] <- check_counts
}
# Combine the results (list format) into a data frame by adding rows. Each
# row reflects the results for one "Province_State" and each column
# represents the vaccine counting method.
result <- do.call(rbind, result) %>% `colnames<-`(make_daily_counts) %>%
  `rownames<-`(test_ir) %>% as.data.frame()

result


## All entries returned TRUE, which is a good sign we can proceed with the 
## isotonic regression method of data smoothing for all of the data.

result = list()
for (i in 1:length(unique(df$Province_State))) {
  # Subset the data set for one "Province_State" entry This time the
  # search space is not restricted.
  subset <- df[df$Province_State %in% unique(df$Province_State)[i], ]
  
  ir_yf = list()
  for (j in 1:length(make_daily_counts)) {
    # Filter out the vaccine count method vector and fit using isoreg().
    ir <- isoreg(subset[, make_daily_counts[j]], y = NULL)
    
    # Save the fitting results predicted y-values. NOTE: In "yf" "f" means
    # "fitted"and "y" denote the prediction based on the isotonic fitting
    # results.
    ir_yf[[j]] <- ir$yf
  }
  
  # Combine the results (list format) into a data frame by adding columns. Each
  # new column represents a different vaccination counting method.
  dataTable <- do.call(cbind, ir_yf) %>%
    `colnames<-`(str_c(make_daily_counts, "_yf")) %>% as.data.frame()
  
  # Separate out the "Date" and "Province_State" columns for the vector of
  # newly fitted results. Everything should be in the same row-order and not
  # require matching to merge. These columns will be used to merge the fitted
  # values back to the main data set.
  rowNames  <- df[df$Province_State %in% unique(df$Province_State)[i], c("Date", "Province_State")]
  
  # Combine the metadata (rowNames) to the built vector of fitted vaccination
  # counts (dataTable).
  result[[i]] <- cbind(rowNames, dataTable)
}
# Combine the results (list format) into a data frame by adding rows. Each
# row reflects the results for one "Province_State" and each column
# represents the vaccine counting method.
result <- do.call(rbind, result) %>%
  `colnames<-`(c("Date", "Province_State", str_c(make_daily_counts, "_yf"))) %>%
  as.data.frame()

# Merge the previous data set with the newly smoothed values, combining by
# unique matches with the "Date" and "Province_State" columns.
df_monotonic <- merge(df, result, by = c("Date", "Province_State"))


## If we plot the same data using the smoothed regression results, we see that we
## indeed have our monotonic data.
df_monotonic %>%
  filter(Province_State %in% us_states) %>%
  ggplot(data = ., aes(x = Date, y = People_at_least_one_dose_yf)) +
      geom_line(aes(color = Province_State)) +
      scale_y_continuous(labels = unit_format(unit = "M", scale = 1e-6)) +
      scale_x_date(date_breaks = "4 month", date_labels =  "%b %Y") +
      labs(title = "Smoothed Data Line Plot - Isotonic `yf` Prediction",
           x = "Daily Updates", y = "People With at Least One Dose `yf`") +
      theme_minimal() + theme(legend.position = "none")


## Now that we have our smoothed, monotonic data we can proceed with generating
## the daily vaccination counts from the cumulative sums without having to worry
## introducing negative values.

# Store the column names reflecting the smoothed, monotonically increasing
# fittings for calculating the daily counts from the cumulative sum.
make_daily_counts_yf <- colnames(df_monotonic[, -c(1:4)]) %>% .[str_detect(., "yf")]

result = list()
for (i in 1:length(unique(df_monotonic$Province_State))) {
  # Subset the data set one for "Province_State" entry.
  subset <- df_monotonic[df_monotonic$Province_State %in% unique(df$Province_State)[i], ]
  
  counts = list()
  for (j in 1:length(make_daily_counts_yf)) {
    # Separate out the vector associated with one vaccination counting method
    # and calculate the difference between the ith and i+1 value. Use the
    # first value in the original vector as the first value in the new vector,
    # and shift difference counts up one position.
    counts[[j]] <- c(subset[, make_daily_counts_yf[j]][1], diff(subset[, make_daily_counts_yf[j]]))
  }
  
  # Combine the results (list format) into a data frame by adding columns. Each
  # column represents the vaccine counting method. Also join the metadata
  # columns to this data set for merging daily counts back to the main data set.
  result[[i]] <- do.call(cbind, counts) %>% as.data.frame() %>%
    cbind(subset[, c("Date", "Province_State")], .) %>%
    `colnames<-`(c(colnames(.)[1:2], str_c(make_daily_counts_yf, "_daily")))
}
# Combine the results (list format) into a data frame by adding rows. Each
# row reflects the results for one "Province_State" and each column
# represents the vaccine counting method.
result   <- do.call(rbind, result)

# Merge the previous data set with the daily counts, combining by unique
# matches with the "Date" and "Province_State" columns.
df_daily <- merge(df_monotonic, result, by = c("Date", "Province_State"))


## We can double check that this calculation worked by plotting the calculated
## daily count values.
df_daily %>%
  filter(Province_State %in% us_states[c(1, 4, 20)]) %>%
  ggplot(data = ., aes(x = Date, y = Doses_admin_yf_daily)) +
      geom_bar(stat = "identity",
               alpha = 0.25, aes(color = Province_State, fill = Province_State)) +
      scale_x_date(date_breaks = "4 month", date_labels =  "%b %Y") +
      labs(title = "Daily Counts Bar Plot - Calculated from `yf`\nFor Alabama, Arkansas, and Maine", 
           x = "Daily Updates", y = "Daily Administration of Vaccines `yf`") +
      theme_minimal()


## Looks good. Now that we have our monotonic smoothed data and back-calculated
## daily counts, we are ready to proceed with finding other opportunities to
## clean or transform the data in preparation for analysis.
## 
## We'll check to confirm that each "Province_State" entry covers the same
## number of days. The Boolean test is TRUE when all provinces have the same
## number of time-stamped entries.


table(df_daily$Province_State, df_daily$Date) %>% 
  sapply(., function(x) x == 1) %>% all()


## We see that not all states/territories have the same number of time-stamped
## entries. Vaccination reports are also not expected to be precise to the
## degree where daily reported updates are meaningful (within error).
##
## Therefore, we will aggregate "Date" by month for each "Province_State". After
## this, we no longer need columns "Date", "UID", or "Country_Region", and so
## we'll remove them from our aggregated data set. Because the raw values
## were not monotonic we will only keep the isotonic regression smoothed values.

# Create a new vector that rounds "Dates" down to the nearest month.
df_daily <- df_daily %>%
  group_by(month = lubridate::floor_date(Date, "month")) %>% as.data.frame()

# In the aggregation, retain only the max value of the cumulative counts.
# Organize aggregation by unique "Province_State" entries and dates
# rounded down to the nearest month.
maxCumulative <- aggregate(. ~ month + Province_State, df_daily[, -c(1, 3:8, 13:16)], max) %>%
  `colnames<-`(c("Month", colnames(.)[-1]))

# Do the same aggregation over the daily counts, but retain the sum.
sumDaily      <- aggregate(. ~ month + Province_State, df_daily[, -c(1, 3:12)], sum) %>%
  `colnames<-`(c("Month", colnames(.)[-1]))

# Merge these two aggregated data sets, combining by unique matches with the
# "Date" and "Province_State" columns.
df_byMonth    <- merge(maxCumulative, sumDaily, by = c("Month", "Province_State"))


## Good sanity check that the rows are aggregated as expected.
head(df_byMonth)
dim(df_byMonth)

# Plot the aggregated cumulative vaccination counts to confirm it retained
# the monotonically increasing feature.
df_byMonth %>%
  filter(Province_State %in% us_states) %>%
  ggplot(data = ., aes(x = Month, y = People_at_least_one_dose_yf)) +
      geom_line(aes(color = Province_State)) +
      scale_y_continuous(labels = unit_format(unit = "M", scale = 1e-6)) +
      scale_x_date(date_breaks = "4 month", date_labels =  "%b %Y") +
      labs(title = "Aggregated Data Line Plot\nMax of Cumulative Sum",
           x = "Monthly Updates", y = "People With at Least One Dose `yf`") +
      theme_minimal() + theme(legend.position = "none")


# Plot the daily vaccination counts, to confirm there are no unexpected features.
df_byMonth %>%
  filter(Province_State %in% us_states) %>%
  ggplot(data = ., aes(x = Month, y = People_at_least_one_dose_yf_daily)) +
      geom_line(aes(color = Province_State)) +
      scale_y_continuous(labels = unit_format(unit = "M", scale = 1e-6)) +
      scale_x_date(date_breaks = "4 month", date_labels =  "%b %Y") +
      labs(title = "Aggregated Data Line Plot\nSum of Daily Counts",
           x = "Monthly Updates", y = "People With at Least One Dose `yf`") +
      theme_minimal() + theme(legend.position = "none")


# Boolean test is TRUE when all provinces have the same number of months
table(df_byMonth$Province_State, df_byMonth$Month) %>% 
  sapply(., function(x) x == 1) %>% all()


## Looks like the aggregation step went as expected. In the last few steps we
## will organize and clean up the data. For convenience, we'll order the
## rows by "Province_State" and then by "Month".
order_by <- c("United States", us_states, us_territories, extra_provinces)

# Start building ordered set with "Province_State" == "United States"
build <- df_byMonth[df_byMonth$Province_State %in% order_by[1] &
                      order(df_byMonth$Month), ]
for (i in 2:length(order_by)) {
  # Iterate the same initial process over the remaining ordered
  # "Province_State" row entries.
  ordered <- df_byMonth[df_byMonth$Province_State %in% order_by[i] &
                          order(df_byMonth$Month), ]
  
  # Add to the previous data set to commit newly ordered row entries.
  build   <- rbind(build, ordered)
}

# Commit the full, row organized data set.
df_byMonth <- build


# Now we will round down all numeric values to the nearest whole integer (i.e.
# the nearest person).
df_byMonth[, -c(1:2)] <- sapply(df_byMonth[, -c(1:2)], floor)

# Clear the row name formatting.
df_byMonth <- df_byMonth %>% `rownames<-`(NULL)



## ----------------------------------------------------------------------------
## SAVE CLEANED DATA FOR ANALYSIS

## Now that basic Extract, Transforming, and Loading (ETL) has been completed,
## and we understand the contents of our data set a little better, we are 
## ready to proceed with plotting.
## 
## For brevity in the tutorial, plotting the cleaned and aggregated data
## will be recorded here and an appended description of what was done
## will be discussed in the tutorial itself.


write.csv(df_byMonth, "Vaccinations Aggregated by Month.csv")




