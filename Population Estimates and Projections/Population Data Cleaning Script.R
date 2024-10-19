## ----------------------------------------------------------------------------
## From Yale's Public Health Data Science and Data Equity (DSDE) Team
##
## Workshop: Getting Started with Git and GitHub
## Authors:  Shelby Golden, M.S.
## Date:     2024-10-18
## 
## R version:    4.4.1
## renv version: 1.0.11


## ----------------------------------------------------------------------------
## SET UP THE ENVIRONMENT
## renv() will install all of the packages and their correct version used here

renv::restore()

library(readxl)
library(readr)
library(tidyr)
library(dplyr)
library(stringr)
library(ggplot2)


## Function to check if an element is not in a vector
"%!in%" <- function(x,y)!('%in%'(x,y))




## ----------------------------------------------------------------------------
## LOAD IN THE DATA

## This data is from the COVID-19 Data Repository by the Center for Systems 
## Science and Engineering (CSSE) at Johns Hopkins University, the U.S.
## Census Bureau, and the U.S. Department of Agriculture. Further details
## on the data can be found in the subdirectory README file.

census_2010to2018_raw <- read_excel("Population Estimates and Projections/U.S. Department of Agriculture_Population Estimates 2010 to 2018_JHU CRC.xls",
                                    skip = 2) %>% as.data.frame()

census_2010to2019_raw <- read_csv("Population Estimates and Projections/U.S. Census Bureau_State Intercensal Dataset 2010 to 2019_Downloaded 10.18.2024.csv") %>% 
                           as.data.frame()

census_2020to2023_raw <- read_excel("Population Estimates and Projections/U.S. Department of Agriculture_Population Estimates 2020 to 2023_Downloaded 10.18.2024.xlsx",
                                    skip = 4) %>% as.data.frame()




## ----------------------------------------------------------------------------
## HARMONIZE THE DATASETS

dim(census_2010to2018_raw)
dim(census_2010to2019_raw)
dim(census_2020to2023_raw)

## While all three data sets are from the same original source (U.S. Census 
## Bureau), they have different column names and dimensions.Therefore, 
## we will need to do some data harmonization to get them into one 
## standardized format.


## ----------------------------------------------------------------------------
## HARMONIZE THE DATASETS - COLUMNS

## All data sets repeat the same type of information for different range
## of years. We first will check that the metadata columns (first few columns
## in the data sets correspond. The metadata ends at the column for 
## the recent census data.
names(census_2010to2018_raw) %>% str_detect(., "CENSUS") %>% which()
names(census_2010to2019_raw) %>% str_detect(., "CENSUS") %>% which()
names(census_2020to2023_raw) %>% str_detect(., "CENSUS") %>% which()

## We see that the metadata columns are not the same length for all three.
names(census_2010to2018_raw) %>% .[1:8]
names(census_2010to2019_raw) %>% .[1:7]
names(census_2020to2023_raw) %>% .[1:7]

## Looks like there are differences between all three data sets, including the 
## two that are expected to be most similar, 2010-2018 and 2020-2023, as they 
## were compiled by the U.S. Department of Agriculture. Between all three, 
## it appears that the following columns constitute the same information: 
## FIPS/SUMLEV, State/STNAME, and Area_Name/CTYNAME. We only need the later two
## and so we will remove the other extraneous metadata columns.

census_2010to2018_subset <- census_2010to2018_raw %>% .[, -c(1, 4:8)] %>% 
                               `colnames<-`(c("state", "county", colnames(.)[-c(1:2)]))
census_2010to2019_subset <- census_2010to2019_raw %>% .[, -c(1:5)] %>% 
                                `colnames<-`(c("state", "county", colnames(.)[-c(1:2)]))
census_2020to2023_subset <- census_2020to2023_raw %>% .[, -c(1, 4:7)] %>% 
                                `colnames<-`(c("state", "county", colnames(.)[-c(1:2)]))


## Now we'll look at those columns that contain the census data for each year.
dates_included <- c("2010", "2011", "2012", "2013", "2014", "2015", "2016", 
                    "2017", "2018", "2019", "2020", "2021", "2022", "2023")

repeated_info <- function(data, date_range){
  ## Function that checks for the number of variables associated with each year
  hits <- c()
  for(i in 1:length(date_range)){
    hits[i] <- names(data)[-c(1:4)] %>% 
                  .[str_detect(., date_range[i], )] %>% 
                  length()
  }
  hits
}

repeated_info(census_2010to2018_subset, dates_included)
repeated_info(census_2010to2019_subset, dates_included)
repeated_info(census_2020to2023_subset, dates_included)


## We see that the first date has a different length of variables associated
## with it compared with the remaining dates represented. Now we check
## that the column names match a reference. For this scenario, we'll assume
## to be census_2020to2023 the reference, but this choice is arbitrary.

check_col_matches <- function(data, names_11, names_16, date_range, with_underscore){
  ## Function that checks for differences to a reference column name
  ## Assumes that the order of information is the same and the
  ## length of the vectors is the same (as shown by function repeated_info())
  result <- list()
  for(i in 1:length(date_range)){
    
    if(with_underscore == TRUE){
      remove_string <- str_c("_", date_range[i])
    }else if(with_underscore == FALSE){
      remove_string <- str_c(date_range[i])
    }
    
    query <- names(data)[-c(1:4)] %>%
               .[str_detect(., date_range[i], )] %>% 
               str_remove(., remove_string) %>% 
               str_to_lower()
    
    
    if(all(is.na(query)) == FALSE){
      if(length(query) == 11){
        not_matched <- which(names_11 %!in% query)
        result[[i]] <- data.frame("Name_length" = 11,
                                  "Ref" = c(names_11[not_matched]), 
                                  "Querry" = c(query[not_matched]))
        
      }else if(length(query) == 16){
        not_matched <- which(names_16 %!in% query)
        result[[i]] <- data.frame("Name_length" = 16,
                                  "Ref" = c(names_16[not_matched]), 
                                  "Querry" = c(query[not_matched]))
      }else{
        result[[i]] <- result[[i]] <- data.frame("Name_length" = "No Querry",
                                                 "Ref" = "NA", 
                                                 "Querry" = "NA")
      }
    }
  }
  do.call(rbind, result)
}


ref_names_11 <- names(census_2020to2023_subset)[-c(1:4)] %>%
                  .[str_detect(., "2020", )] %>% 
                  str_remove(., str_c("_", "2020")) %>% 
                  str_to_lower()

ref_names_16 <- names(census_2020to2023_subset)[-c(1:4)] %>%
                  .[str_detect(., "2021", )] %>% 
                  str_remove(., str_c("_", "2021")) %>% 
                  str_to_lower()


check_col_matches(census_2010to2018_subset, ref_names_11, ref_names_16, dates_included, TRUE)
check_col_matches(census_2010to2019_subset, ref_names_11, ref_names_16, dates_included, FALSE)


## Looks like there are a good number of differences for exact comparison,
## irrespective of the date. Visual inspection implies that the column name
## segments have the same order of information and that variations in the 
## column names imply the same information is represented. All that obviously 
## seems to differ is the exact string used.

## Now we'll coerce the column names to be consistent with the reference names.

change_col_names <- function(data, names_11, names_16, date_range){
  ## Function that checks for differences to a reference column name
  ## Assumes that the order of information is the same and the
  ## length of the vectors is the same (as shown by function repeated_info())
  
  subset = data[-c(1:4)]
  for(i in 1:length(date_range)){
    
    query <- str_detect(names(subset), date_range[i]) %>% which()
    
    if(length(query) > 0){
      if(length(query) == 11){
        colnames(subset)[query] <- str_c(names_11, "_", date_range[i])
      }else if(length(query) == 16){
        colnames(subset)[query] <- str_c(names_16, "_", date_range[i])
      }
    }
    
  }
  cbind(data[c(1:4)], subset) %>% 
    `colnames<-`(c(colnames(.)[c(1:2)], str_to_lower(colnames(.)[c(3:4)]), 
                   colnames(.)[-c(1:4)]))
}

census_2010to2018 <- change_col_names(census_2010to2018_subset, ref_names_11, ref_names_16, dates_included)
census_2010to2019 <- change_col_names(census_2010to2019_subset, ref_names_11, ref_names_16, dates_included)
census_2020to2023 <- change_col_names(census_2020to2023_subset, ref_names_11, ref_names_16, dates_included)

## Final correction of columns
colnames(census_2010to2019)[3:4] <- c("census_2010_pop", "estimates_base_2010")




## ----------------------------------------------------------------------------
## HARMONIZE THE DATASETS - ROWS

## One likely cause for differences in row entries is the presence or absence
## of counties within states. For our analysis, we only require the entries
## that are states, territories, or the District of Columbia.
us_state        = datasets::state.name
us_territories  = c("American Samoa", "Guam", "Northern Mariana Islands", 
                    "Puerto Rico", "Virgin Islands")
us_district     = "District of Columbia"
us_allAreas     = c(us_state, us_territories, us_district, "United States")


## And now we filter the rows that include these entries and see that the
## doing this harmonizes the row entries.
census_2010to2018_filtered <- census_2010to2018 %>%
                                filter(.data = ., county %in% us_allAreas)

census_2010to2019_filtered <- census_2010to2019 %>%
                                filter(.data = ., county %in% us_allAreas)

census_2020to2023_filtered <- census_2020to2023 %>%
                                filter(.data = ., county %in% us_allAreas)


c("Number of rows:", nrow(census_2010to2018_filtered), 
  nrow(census_2010to2019_filtered), nrow(census_2020to2023_filtered))


## This is better, but we see that the 2010-2019 Census data is lacking two entries:
census_2010to2018_filtered$county %>% .[. %!in% census_2010to2019_filtered$county]

## We'll confirm that the entries that match for states are what we expect to see:
census_2010to2018_filtered$county %>% .[. %in% census_2010to2019_filtered$county & 
                                           . %in% us_state] %>% length() == 50

## We'll confirm that the entries that do nit match for states are what we 
## expect to see:
census_2010to2018_filtered$county %>% .[. %in% census_2010to2019_filtered$county & 
                                             . %!in% us_state]


## Two entries, for United States and Puerto Rico, are missing from the
## 2010-2019 Census data. District of Columbia seems to be added in twice.
## Therefore, we see that there are no territories included, only states
## and the District of Columbia.

## We will check that the duplicate entries are indeed the same before 
## removing one.

check_matched_row_entries <- function(data, query_filter){
  ## Function to confirm row entries satisfying a filter are the same
  check = data %>% .[.$county == query_filter, ] %>% 
              apply(., 2, function(x) length(unique(x)) == 1) %>% all()
  if(check == TRUE){
    print("All entries are the same.")
  }else{
    print("Some entries are not the same.")
  
  }
}

check_matched_row_entries(census_2010to2018_filtered, "District of Columbia")
check_matched_row_entries(census_2010to2019_filtered, "District of Columbia")
check_matched_row_entries(census_2020to2023_filtered, "District of Columbia")


## We see that the entries are the same, so we can remove one of the duplicates.
census_2010to2018_filtered$county[census_2010to2018_filtered$county 
                                  %in% "District of Columbia"] <- c("District of Columbia", "District of Columbia_remove")
census_2010to2019_filtered$county[census_2010to2019_filtered$county 
                                  %in% "District of Columbia"] <- c("District of Columbia", "District of Columbia_remove")
census_2020to2023_filtered$county[census_2020to2023_filtered$county 
                                  %in% "District of Columbia"] <- c("District of Columbia", "District of Columbia_remove")


census_2010to2018_filtered <- census_2010to2018_filtered %>% 
                                filter(.data = ., county != "District of Columbia_remove" &
                                       county != "Puerto Rico")
census_2010to2019_filtered <- census_2010to2019_filtered %>% 
                                filter(.data = ., county != "District of Columbia_remove")
census_2020to2023_filtered <- census_2020to2023_filtered %>% 
                                filter(.data = ., county != "District of Columbia_remove" &
                                         county != "Puerto Rico")


## The final step is to check if the sum of the entries under United States are the
## same for each column if calculated by summing the states.
us_sum_test <- function(data, no_na = TRUE){
  ## Function to check if the sum of the entries under United States are the
  ## same for each row entry if calculated by summing the states.
  sum_test <- rbind(data %>% .[.$county %!in% "United States", -c(1:4)] %>% 
                            sapply(., function(x) sum(x)),
                        data %>% .[.$county == "United States", -c(1:4)] %>% 
                            sapply(., function(x) sum(x))) %>% 
                    `row.names<-`(c("States and Territories", "United States")) %>%
                    as.data.frame()
  
  if(no_na == TRUE){
    output = sum_test %>% .[sapply(., function(x) !any(is.na(x)))]
    check = output %>% apply(., 2, function(x) length(unique(x)) == 1) %>% all()
    
  }else{
    output = sum_test %>% .[sapply(., function(x) any(is.na(x)))]
    check = output %>% apply(., 2, function(x) length(unique(x)) == 1) %>% all()
  }
  
  if(check == TRUE){
    list(print("All entries are the same."), output)
  }else{
    list(print("Some entries are not the same."), output)
    
  }
}


us_sum_test(census_2010to2018_filtered, TRUE)[[1]]
us_sum_test(census_2010to2018_filtered, FALSE)[[1]]
us_sum_test(census_2020to2023_filtered, TRUE)[[1]]
us_sum_test(census_2010to2018_filtered, FALSE)[[1]]


## We see that there are some NA's, and it appears that the sum of entries with 
## and "r" preceding do not match when summed. Therefore, these will be set to
## NA. All others will be adjusted to the sum over the states.

empty_2010to2019_US <- rep(NA, ncol(census_2010to2019_filtered))

## Make the 2010-2019 Census data columns with an "r" NA for county == United States
add_val <- str_detect(names(census_2010to2019_filtered), "r_", negate = TRUE) %>% which()

empty_2010to2019_US[add_val] <- census_2010to2019_filtered[, add_val] %>% 
                                    .[, -c(1:2)] %>% sapply(., sum) %>% c(NA, NA, .)

add_row <- t(as.data.frame(empty_2010to2019_US)) %>% `colnames<-`(names(census_2010to2019_filtered))

census_2010to2019_filtered <- rbind(add_row, census_2010to2019_filtered) %>% 
                                  `rownames<-`(NULL)
census_2010to2019_filtered[1, 1:2] <- c("United States", "United States")



## Now that the rows and columns correspond where they need to, and unnecessary
## rows have been removed by county, we can remove the redundant column 
## denoting the state
census_2010to2018 <- census_2010to2018_filtered %>% .[, -1] %>% 
                        `colnames<-`(c("state", colnames(.)[-1]))
census_2010to2019 <- census_2010to2019_filtered %>% .[, -1] %>% 
                        `colnames<-`(c("state", colnames(.)[-1]))
census_2020to2023 <- census_2020to2023_filtered %>% .[, -1] %>% 
                        `colnames<-`(c("state", colnames(.)[-1]))



## ----------------------------------------------------------------------------
## CHECK 2010 TO 2018 ARE THE SAME
subset_2010to2019 <- census_2010to2019 %>% .[str_detect(names(.), "2019", negate = TRUE)]

dim(census_2010to2018)
dim(subset_2010to2019)


## It is not necessary for the data set to be exact, but we will check that
## there are not any obvious differences.
all.equal(census_2010to2018, subset_2010to2019, tolerance = 0.5)
all.equal(census_2010to2018, subset_2010to2019, tolerance = 1e-3)

## For the most part, it appears that differences are within a reasonable error
## to assume they are similar. Some correspondences are stronger than others.
## Calculated values seem to have the most error, but this could be because
## their algorithm adjusts each year it is run. Therefore, we will proceed with 
## the 2010-2019 data set in place of the 2010-2018 one.




## ----------------------------------------------------------------------------
## COMBINE THE 2010-2019 AND 2020-2023 DATASET

## Double check the nrow are the same as census_2010to2019
dim(census_2020to2023)


## Using the reference names, we will build out the combined data set. The
## goal is to organize the information the same way as the raw data sets.
## We'll check which names are repeated in the different sub column sets.
## 
## As was shown earlier, the base census year included 11 columns of data
## while the remaining intercensal column estimates included an additional
## 5 columns of data.
ref_names_11[ref_names_11 %!in% ref_names_16]

## Looks like only one column name differs. We'll create one master reference
## column list appending the missing column names to ref_names_11.
ref_names <- c(ref_names_11, ref_names_16[ref_names_16 %!in% ref_names_11])


## The following for loop series will combine the two data sets by column name
## then date so that the column are organized in the same manner as the 
## raw formats.

build <- cbind(census_2010to2019[, c(1:3)], census_2020to2023[, 2:3])
for(i in 1:length(ref_names)){
  
  for(j in 1:length(dates_included)){
    
    if(dates_included[j] %in% c(dates_included[1:10])){
      data = census_2010to2019
      
    }else if(dates_included[j] %in% c(dates_included[11:14])){
      data = census_2020to2023
    }
    
    
    index <- str_detect(colnames(data), 
                        str_c("\\b", ref_names[i], "_", dates_included[j]))
    build <- cbind(build, data[, index, drop = FALSE] )
  }
  
}

census_2010to2023 <- build




## ----------------------------------------------------------------------------
## FINAL CHECK FOR OBVIOUS ERRORS WITH HARMONIZATION
new_2010to2023 <- census_2010to2023
raw_2010to2019 <- census_2010to2019_raw
raw_2020to2023 <- census_2020to2023_raw


## The United States row was calculated for some dates above. We'll compare
## the state-level data only. Each of these is already organized alphabetically,
## and so no additonal column ordering is require.
new_2010to2023 <- new_2010to2023 %>% filter(state %in% us_state)
raw_2010to2019 <- raw_2010to2019 %>% filter(CTYNAME %in% us_state)
raw_2020to2023 <- raw_2020to2023 %>% filter(Area_Name %in% us_state)


## The unedited version of the 2010-2019 and 2020-2023 data sets include a "_raw"
## at the end of their names. Each column name has been modified since these
## were imported. If we strip differences by removing all non-alphanumeric
## characters and converting to lower case, we can see if there are any
## not-easily reconciled discrepancies.

colnames(new_2010to2023) <- colnames(new_2010to2023) %>% str_replace_all(., "[^[:alnum:]]", "") %>% str_to_lower()
colnames(raw_2010to2019) <- colnames(raw_2010to2019) %>% str_replace_all(., "[^[:alnum:]]", "") %>% str_to_lower()
colnames(raw_2020to2023) <- colnames(raw_2020to2023) %>% str_replace_all(., "[^[:alnum:]]", "") %>% str_to_lower()


## It appears that column names associated with natural change are not included
## in either the 2010-2019 or 2020-2023 data sets.
a <- colnames(new_2010to2023)
b <- colnames(raw_2010to2019)
c <- colnames(raw_2020to2023)

a[a %!in% b & a %!in% c]

b[b %!in% a]
c[c %!in% a]


## Not considering the preceding metadata columns, which were removed early on
## in the process, it appears the principal difference is with the 2010-2019
## data set column names for natural change, which are called natural increase
## instead. We'll replace those so that they are the same for our 
## column-by-column comparison.

colnames(raw_2010to2019)[str_detect(colnames(raw_2010to2019), "natural")] <- 
      colnames(raw_2010to2019)[str_detect(colnames(raw_2010to2019), "natural")] %>% 
      str_replace(., "inc", "chg")


## In the column-by-column comparison, column names are assumed to be ordered
## the same way and that there are no extraneous columns including the called
## dates. We see from above the some metadata columns include dates, and so we'll
## exclude those so that there will be no problem.

problem_columns <- c[c %!in% a][3:6]
raw_2020to2023  <- raw_2020to2023[, colnames(raw_2020to2023) %!in% problem_columns]


## Now we are ready to do the column-by-column comparison.

result <- list()
for(i in 1:10){
  reference <- raw_2010to2019[, str_detect(colnames(raw_2010to2019), dates_included[i])]
  query     <- new_2010to2023[, str_detect(colnames(new_2010to2023), dates_included[i])]
  
  result[[i]] <- all.equal(reference, query, tolerance = 0.5)
}

test_result <- do.call(rbind, result) %>% as.data.frame()


result <- list()
for(i in 11:14){
  reference <- raw_2020to2023[, str_detect(colnames(raw_2020to2023), dates_included[i])]
  query     <- new_2010to2023[, str_detect(colnames(new_2010to2023), dates_included[i])]
  
  result[[i]] <- all.equal(reference, query, tolerance = 0.5)
}

test_result <- do.call(rbind, result[11:14]) %>% as.data.frame() %>% rbind(test_result, .) %>% 
                  `rownames<-`(c(dates_included)) %>% `colnames<-`(c("Corresponding Columns"))


## We see that all subsection of columns corresponding to the dates are exactly
## identical, with no discrepancy detected. Thus we feel confident proceeding
## with saving the harmonized census data. NOTE: The number of columns vary 
## from 11 to 16 corresponding columns of details.
test_result



## ----------------------------------------------------------------------------
## WRITING THE COMBINED FILE

write.csv(census_2010to2023, "Population Estimates and Projections/US_Census Population Estimates_2010 to 2023.csv")







