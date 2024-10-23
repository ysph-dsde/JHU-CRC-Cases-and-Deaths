## ----------------------------------------------------------------------------
## From Yale's Public Health Data Science and Data Equity (DSDE) Team
##
## Workshop: Getting Started with Git and GitHub
## Authors:  Shelby Golden, M.S.
## Date:     2024-10-18
## 
## R version:    4.4.1
## renv version: 1.0.11
## 
## Description: This script harmonizes U.S. population census and intercensal
##              year population estimates from two different sources: the
##              U.S. Department of Agriculture, who processes and re-formats
##              annual vintages from the U.S. Census Bureau, and the U.S. Census 
##              Bureau anual release of vintage updates as well. Additional
##              details about the data sources and pertinet links can be found
##              in the README for this projects GitHub repo subdirectory.

## ----------------------------------------------------------------------------
## SET UP THE ENVIRONMENT

## renv() will install all of the packages and their correct version used here.
renv::restore()

## Load in the R packages used in this script from the project library.
library(readxl)
library(readr)
library(tidyr)
library(dplyr)
library(stringr)


## This function will check if an element is not in a vector.
"%!in%" <- function(x,y)!('%in%'(x,y))


## This function checks for the number of variables associated with each year
## in the provided date range.
repeated_info <- function(data, date_range) {
  hits <- c()
  for (i in 1:length(date_range)) {
    # Find the number of column names with the current date included. Save
    # the length of this subset of names.
    hits[i] <- names(data) %>% .[str_detect(., date_range[i], )] %>% length()
  }
  hits
}


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


## This function checks for column name differences compared with a reference.
## It assumes that the order of information is the same and the length of the 
## vectors is the same (as shown by function repeated_info()).
check_col_matches <- function(data, names_11, names_16, date_range, with_underscore) {
  result <- list()
  for (i in 1:length(date_range)) {
    
    # Some of the data we are harmonizing has an underscore while others do
    # not have an underscore.
    if (with_underscore == TRUE) {
      # Prompt to remove the date with a preceding underscore.
      remove_string <- str_c("_", date_range[i])
    } else if (with_underscore == FALSE) {
      # Prompt to remove the date without a preceding underscore.
      remove_string <- str_c(date_range[i])
    }
    
    # Exclude the non-repeated columns (metadata and census year
    # population and base estimates) and find columns with the prompted year.
    query <- names(data)[-c(1:4)] %>% .[str_detect(., date_range[i], )] %>%
      # Remove the year based on the if statement above and covert all names
      # to lowercase.
      str_remove(., remove_string) %>% str_to_lower()
    
    
    if (all(is.na(query)) == FALSE) {
      # If the length of column names subset is 11, then:
      if (length(query) == 11) {
        # Compare names that are not matched with the reference column
        # names vector of the same length.
        not_matched <- which(names_11 %!in% query)
        
        # Store the number of columns, the names not matched to the reference,
        # and the names not matched in the query.
        result[[i]] <- data.frame(
          "Name_length" = 11,
          "Ref" = c(names_11[not_matched]),
          "Querry" = c(query[not_matched])
        )
        
      # If the length of the column names subset is 16, then do the same
      # comparison as for column names length of 11 above.
      } else if (length(query) == 16) {
        not_matched <- which(names_16 %!in% query)
        result[[i]] <- data.frame(
          "Name_length" = 16,
          "Ref" = c(names_16[not_matched]),
          "Querry" = c(query[not_matched])
        )
        
      # If there are no matches, then store "NA".
      } else{
        result[[i]] <- result[[i]] <- data.frame(
          "Name_length" = "No Querry",
          "Ref" = "NA",
          "Querry" = "NA"
        )
      }
    }
  }
  # Combine the results as added rows to a new data set.
  do.call(rbind, result)
}


## This function standardizes the column naming format to a reference. It
## allows sub setting the column names with varying naming standards between
## raw data sets for customization.
change_col_names <- function(data, subset_range, custom_subset_names, names_to) {
  
  # Separate out the columns where the repeated column names occurs.
  subset = colnames(data)[-subset_range]
  
  # Generate a universal names search that is stripped of formatting and
  # link to the target naming convention.
  universal <- data.frame(
    "Target" = names_to,
    "Search" = names_to %>%
      # Remove the non-alphabetical characters, spaces, and make the string
      # lower case for matching.
      str_replace_all(., "[^[:alnum:]]|[0-9]", "") %>% str_to_lower()
  )
  
  # Store the dates associated with each column for appending later.
  ordered_dates <- subset %>% str_extract(., "[0-9]{1,4}$")
  
  # Remove the non-alphabetical characters, spaces, and make the string
  # lower case for matching.
  subset <- subset %>% str_replace_all(., "[^[:alnum:]]|[0-9]", "") %>% str_to_lower()
  
  for (i in 1:nrow(universal)) {
    # Find the column indices where the un-formatted string matches.
    query <- str_detect(subset, str_c("\\b", universal[i, "Search"], "\\b")) %>% which()
    
    # Change the column names at those indices to the target format.
    subset[query] <- universal[i, "Target"]
  }
  
  # Piece together the naming elements and reintroduce the date associated.
  colnames(data) <- c(custom_subset_names, str_c(subset, "_", ordered_dates))
  
  data
}


## This function checks if the values for the United States are the same as
## the column sums over the values for all states.
us_sum_test <- function(data, no_na = TRUE) {
  # Only looking at the non-meta data columns, sum over the columns, excluding
  # the row entry where County == "United States".
  sum_test <- rbind(data %>% .[.$County %!in% "United States", -c(1:4)] %>%
                      sapply(., function(x) sum(x)),
                    # Save values for the County == "United States" row only.
                    data %>% .[.$County == "United States", -c(1:4)]) %>%
    # Adjust the row names to reflect which rows the sums reflect.
    `row.names<-`(c("States", "United States")) %>% as.data.frame()
  
  # We expect that columns with an NA in them might not match with the
  # sum over any of the other columns, and so we separate those out.
  if (no_na == TRUE) {
    # Search for NA's column-by-column and subset the data for those
    # where no NA was found.
    subset = sum_test %>% .[sapply(., function(x) ! any(is.na(x)))]
    
  } else if (no_na == FALSE) {
    # Similar as for no_na == TRUE, except subset when an NA is found.
    subset = sum_test %>% .[sapply(., function(x) any(is.na(x)))]
  }
  
  # If the two rows for any column is equal, then they will not be unique.
  result = subset %>% apply(., 2, function(x) length(unique(x)) == 1)
  # The Boolean check is all-or-nothing.
  check  = result %>% all()
  # Subset the results to show columns where the sums do not match.
  output = subset[, !result]
  
  
  # If statement to condition the output based on the results.
  if (check == TRUE) {
    list(print("All entries are the same."), output)
    
  } else{
    list(print("Some entries are not the same."), output)
  }
}




## ----------------------------------------------------------------------------
## LOAD IN THE DATA

## This data is from the COVID-19 Data Repository by the Center for Systems 
## Science and Engineering (CSSE) at Johns Hopkins University, the U.S.
## Census Bureau, and the U.S. Department of Agriculture. Further details
## on the data can be found in this projects GitHub repo's subdirectory 
## README file.

census_2010to2018_raw <- read_excel(
  "Population Estimates and Projections/U.S. Department of Agriculture_Population Estimates 2010 to 2018_JHU CRC.xls",
  skip = 2
) %>% as.data.frame()

census_2010to2019_raw <- read_csv(
  "Population Estimates and Projections/U.S. Census Bureau_State Intercensal Dataset 2010 to 2019_Downloaded 10.18.2024.csv"
) %>% as.data.frame()

census_2020to2023_raw <- read_excel(
  "Population Estimates and Projections/U.S. Department of Agriculture_Population Estimates 2020 to 2023_Downloaded 10.18.2024.xlsx",
  skip = 4
) %>% as.data.frame()




## ----------------------------------------------------------------------------
## HARMONIZE THE DATASETS

dim(census_2010to2018_raw)
dim(census_2010to2019_raw)
dim(census_2020to2023_raw)

## While all three data sets are unlimately from the same original source (U.S. 
## Census Bureau), they have different column names and dimensions. Therefore, 
## we will need to do some data harmonization to get them into one 
## standardized format.




## ----------------------------------------------------------------------------
## HARMONIZE THE DATASETS - COLUMNS

## All data sets repeat the same type of information for different range
## of years. We first will check that the metadata columns (first few columns
## in the data sets) correspond. The metadata ends at the column for 
## the recent census year of observed values.

names(census_2010to2018_raw) %>% str_detect(., "CENSUS") %>% which()
names(census_2010to2019_raw) %>% str_detect(., "CENSUS") %>% which()
names(census_2020to2023_raw) %>% str_detect(., "CENSUS") %>% which()


## We see that the metadata columns are not the same length for all three.
names(census_2010to2018_raw) %>% .[1:8]
names(census_2010to2019_raw) %>% .[1:7]
names(census_2020to2023_raw) %>% .[1:7]


## Looks like there are differences between all three data sets, including the 
## two that are expected to be most similar, 2010 to 2018 and 2020 to 2023, as 
## they were compiled by the U.S. Department of Agriculture. Between all three, 
## it appears that the following columns constitute the same information: 
## FIPS/SUMLEV, State/STNAME, and Area_Name/CTYNAME. We only need the later two
## and so we will remove the other extraneous metadata columns.

census_2010to2018_subset <- census_2010to2018_raw %>% .[, -c(1, 4:8)] %>%
  `colnames<-`(c("state", "county", colnames(.)[-c(1:2)]))

census_2010to2019_subset <- census_2010to2019_raw %>% .[, -c(1:5)] %>%
  `colnames<-`(c("state", "county", colnames(.)[-c(1:2)]))

census_2020to2023_subset <- census_2020to2023_raw %>% .[, -c(1, 4:7)] %>%
  `colnames<-`(c("state", "county", colnames(.)[-c(1:2)]))


## Now we'll look at those columns that contain the intercensal estimates for
## each year, including the base census year (2010 or 2020 as indicated).
dates_included <- str_c("20", 10:23)

repeated_info(census_2010to2018_subset[-c(1:4)], dates_included)
repeated_info(census_2010to2019_subset[-c(1:4)], dates_included)
repeated_info(census_2020to2023_subset[-c(1:4)], dates_included)


## We see that the first date has a different length of variables associated
## with it compared with the remaining dates. We expect that the same kinds of 
## information is being repeated for each intercensal date predicted, with 
## some minor variation for the base census year. We can confirm by checking 
## that each date's set of variables are about the same kinds of information.

# Create a data frame, containing the following column elements:
unique_names <- cbind(
  # Removing the non-repeated column elements (metadata and census year 
  # population and base estimates), save the column names.
  census_2010to2018_subset[-c(1:4)] %>% colnames() %>% 
    # Remove the numeric values, which would strip the date designation, and
    # count the number of occurrences.
    str_replace_all(., "[0-9]", "") %>% table() %>% 
    # Reformat the reported results.
    as.data.frame() %>% `colnames<-`(c("2010to2018", "Freq")),
  
  # Repeat the same steps for the other two data sets.
  census_2010to2019_subset[-c(1:4)] %>% colnames() %>% 
    str_replace_all(., "[0-9]", "") %>% table() %>% 
    as.data.frame() %>% `colnames<-`(c("2010to2019", "Freq")),
  census_2020to2023_subset[-c(1:4)] %>% colnames() %>% 
    str_replace_all(., "[0-9]", "") %>% table() %>% 
    as.data.frame() %>% `colnames<-`(c("2020to2023", "Freq"))
)

unique_names


## This table confirms that, with the exception of a few variables, each
## intercensal year has the same columns of information reported. Columns with
## one less frequency count are ones that are only included for intercensal
## dates, while those with the max counts are also represented for the base
## census year. The "GQ Estimates Base" variable only occurs once in the
## data set, likely because it reports information for the reference census 
## year only.

# Pull the column names associated with the census year, which should have 11 names.
census_year_names    <- colnames(census_2020to2023_subset)[str_detect(colnames(census_2020to2023_subset), "_2020")] %>%
  # Remove the non-alphanumeric characters and numbers and trim any extra spacing.
  str_replace_all(., "[^[:alnum:]]|[0-9]{1,4}", " ") %>% str_trim(., side = c("both"))

# Pull the column names associated with the census year, which should have 16 names.
extimated_year_names <- colnames(census_2020to2023_subset)[str_detect(colnames(census_2020to2023_subset), "_2021")] %>%
  # Remove the non-alphanumeric characters and numbers and trim any extra spacing.
  str_replace_all(., "[^[:alnum:]]|[0-9]{1,4}", " ") %>% str_trim(., side = c("both"))


# Generate the column name convention for each year.
use_names <- c(census_year_names[-c(1:2)], extimated_year_names[extimated_year_names %!in% census_year_names]) %>% 
  # Capitalize each word in the string by the title format.
  str_to_title() %>% str_replace_all(., " ", "_")


## Now we'll change the column names to match the desired format.
census_2010to2018 <- change_col_names(
  census_2010to2018_subset, c(1:4),
  c("State", "County", "Census_Pop_2010", "Estimates_Base_2010"), use_names)

census_2010to2019 <- change_col_names(
  census_2010to2019_subset, c(1:4),
  c("State", "County", "Census_Pop_2010", "Estimates_Base_2010"), use_names)

census_2020to2023 <- change_col_names(
  census_2020to2023_subset, c(1:4),
  c("State", "County", "Census_Pop_2020", "Estimates_Base_2020"), use_names)


## Just as a sanity check, we will check to ensure all the column names
## now adhere to the expected formatting.

# Strip the column names of their dates and check for matches with unique
# column names.
colnames(census_2010to2018)[-c(1:4)] %>% str_replace_all(., "_[0-9]{1,4}", "") %>% 
  unique(.) %>% .[. %!in% use_names]

colnames(census_2010to2019)[-c(1:4)] %>% str_replace_all(., "_[0-9]{1,4}", "") %>% 
  unique(.) %>% .[. %!in% use_names]

colnames(census_2020to2023)[-c(1:4)] %>% str_replace_all(., "_[0-9]{1,4}", "") %>% 
  unique(.) %>% .[. %!in% use_names]


## We see that the 2010 to 2018 and 2010 to 2019 data sets have two column
## names that were not changed: naturalinc which should be Natural_Chg and
## rnaturalinc which should be R_Natural_Chg. We will string replace these.

colnames(census_2010to2018)[str_detect(colnames(census_2010to2018), "\\bnatural")] <-
  # Find all occurrences of naturalinc (with a regex boundary to avoid
  # accidental matches with rnaturalinc).
  colnames(census_2010to2018)[str_detect(colnames(census_2010to2018), "\\bnatural")] %>%
  # Replace these occurrences with Natural_Chg.
  str_replace(., "naturalinc", "Natural_Chg")

colnames(census_2010to2018)[str_detect(colnames(census_2010to2018), "rnatural")] <-
  colnames(census_2010to2018)[str_detect(colnames(census_2010to2018), "rnatural")] %>%
  str_replace(., "rnaturalinc", "R_Natural_Chg")

# Repeat the process for the 2010 to 2019 data set.
colnames(census_2010to2019)[str_detect(colnames(census_2010to2019), "\\bnatural")] <-
  colnames(census_2010to2019)[str_detect(colnames(census_2010to2019), "\\bnatural")] %>%
  str_replace(., "naturalinc", "Natural_Chg")

colnames(census_2010to2019)[str_detect(colnames(census_2010to2019), "rnatural")] <-
  colnames(census_2010to2019)[str_detect(colnames(census_2010to2019), "rnatural")] %>%
  str_replace(., "rnaturalinc", "R_Natural_Chg")


## Now we will double check that our direct changes worked.
colnames(census_2010to2018)[-c(1:4)] %>% str_replace_all(., "_[0-9]{1,4}", "") %>% 
  unique(.) %>% .[. %!in% use_names]

colnames(census_2010to2019)[-c(1:4)] %>% str_replace_all(., "_[0-9]{1,4}", "") %>% 
  unique(.) %>% .[. %!in% use_names]

## Looks like they did. Now we are ready to proceed with harmonizing differences
## with the row entries.




## ----------------------------------------------------------------------------
## HARMONIZE THE DATASETS - ROWS

## Recall that above we found that the number of rows differed between the
## raw format of these data sets. One likely cause for differences is the 
## presence or absence of counties within states. We expect that the census
## data from all three sources (which ultimately comes from the U.S. Census
## Bureau) will have total U.S. counts, counts by state, the District of
## Columbia., and Puerto Rico.

us_states = c("United States", datasets::state.name, 
              "District of Columbia", "Puerto Rico")


## And now we filter the rows that include these states and territories.
census_2010to2018_filtered <- census_2010to2018[census_2010to2018$County %in% us_states, ]
census_2010to2019_filtered <- census_2010to2019[census_2010to2019$County %in% us_states, ]
census_2020to2023_filtered <- census_2020to2023[census_2020to2023$County %in% us_states, ]

c("Number of rows:", nrow(census_2010to2018_filtered), 
  nrow(census_2010to2019_filtered), nrow(census_2020to2023_filtered))


## This is better, but we see that the 2010 to 2019 Census data is lacking one
## entry and the other two have one extra entry.
census_2010to2018_filtered$County %>% .[. %!in% census_2010to2019_filtered$County]

## Looks like the total counts for all U.S. states and Puerto Rico is missing
## from the 2010 to 2019 vintage. We'll confirm that the 2010 to 2019 and the
## 2020 to 2023 census data have the same entries.
census_2010to2019_filtered$County %>% .[. %!in% census_2020to2023_filtered$County]

## Treating 2010 to 2019 and the 2020 to 2023 census data as the same vector,
## we'll confirm that all entries include the expected entries in the
## us_states vector.
us_states %>% .[. %in% census_2020to2023_filtered$County] %>% length(.) == 53

## All of the states and territories are reflected in these two census data
## sets, implying that there is a duplicated entry.
table(census_2020to2023_filtered$County) %>% .[. > 1]

## The District of Columbia seems to be added in twice. We will check that the 
## duplicate entries are indeed the same before removing one.

check_matched_row_entries(census_2010to2018_filtered, "County", "District of Columbia")
check_matched_row_entries(census_2010to2019_filtered, "County", "District of Columbia")
check_matched_row_entries(census_2020to2023_filtered, "County", "District of Columbia")


## We see that the entries are the same, so we can remove one of the duplicates
## at random. As stated in the README file, the input data and methodologies
## used to generate updated vintages for each subsequent intercensal year 
## differs from year-to-year. In the interest of data integrity, the entry for
## Puerto Rico will be removed from the harmonized data set, as its 2019 and 
## preceding vintages were not included.
census_2010to2018_filtered <- distinct(census_2010to2018_filtered)
census_2010to2019_filtered <- distinct(census_2010to2019_filtered)
census_2020to2023_filtered <- distinct(census_2020to2023_filtered)

census_2010to2018_filtered <- census_2010to2018_filtered %>%
  filter(.data = ., County != "Puerto Rico")
census_2020to2023_filtered <- census_2020to2023_filtered %>%
  filter(.data = ., County != "Puerto Rico")


## Now all that remains is to see if there is an opportunity to calculate the
## total values for the United States row missing from the 2010 to 2019 data.
## set. We do this by checking if the sum of the entries under United States
## are the same for each column if calculated by summing the states.

us_sum_test(census_2010to2018_filtered, TRUE)[[1]]
us_sum_test(census_2010to2018_filtered, FALSE)[[1]]
us_sum_test(census_2020to2023_filtered, TRUE)[[1]]
us_sum_test(census_2020to2023_filtered, FALSE)[[1]]

## Even when we separate out column vectors with an NA we get values that
## are not matches. We can check what those are:

us_sum_test(census_2010to2018_filtered, FALSE)[[2]] %>% colnames()
us_sum_test(census_2020to2023_filtered, TRUE)[[2]] %>% colnames()
us_sum_test(census_2020to2023_filtered, FALSE)[[2]] %>% colnames()


## We see that columns where the name starts with an "R_" do not reflect the
## column sum over all states. Therefore, we will set these to NA in our 
## constructed row and report the sum for the remaining columns.

# Start with an empty vector full of NA values.
empty_2010to2019_US <- rep(NA, ncol(census_2010to2019_filtered))

# Exclude the indices that match columns where the name includes an "R_"
add_val <- str_detect(names(census_2010to2019_filtered), "R_", negate = TRUE) %>% which()

# Extract out the columns where the sum over states will be calculated.
empty_2010to2019_US[add_val] <- census_2010to2019_filtered[, add_val] %>%
  # Exclude the two preceding columns with character values and do the
  # column-wise sum. Join with the correct entries for the character columns.
  .[, -c(1:2)] %>% sapply(., sum) %>% c("United States", "United States", .)

# Organize the vector of sums into a data frame with column names for merging.
add_row <- t(as.data.frame(empty_2010to2019_US)) %>% `colnames<-`(names(census_2010to2019_filtered))

# Add the newly created row to the first row-position in the full data set.
census_2010to2019_filtered <- rbind(add_row, census_2010to2019_filtered) %>%
  # Clear the row names.
  `rownames<-`(NULL)

# Some columns will be coerced in a character classification. This will revert
# those columns back to numeric.
census_2010to2019_filtered[, -c(1:2)] <- sapply(census_2010to2019_filtered[, -c(1:2)], as.numeric)


## Now that the rows and columns correspond where they need to, and unnecessary
## rows have been removed by county, we can remove the redundant column 
## denoting the state.
census_2010to2018 <- census_2010to2018_filtered %>% .[, -2]
census_2010to2019 <- census_2010to2019_filtered %>% .[, -2]
census_2020to2023 <- census_2020to2023_filtered %>% .[, -2]



## ----------------------------------------------------------------------------
## CHECK 2010 TO 2018 ARE THE SIMILAR

## As a sanity check, we will see to what degree the 2010 to 2018 vintage
## and 2010 to 2019 vintage compare. As explained in the U.S. Census Bureau's
## methodology, these are recalculated with each successive intercensal year,
## and so we expect there will be some differences. We also don't expect
## there to be a large change.

# Exclude the columns associated with the year 2019.
subset_2010to2019 <- census_2010to2019 %>% .[str_detect(names(.), "2019", negate = TRUE)]

# Double check this yields data sets of equal dimensions.
dim(census_2010to2018)
dim(subset_2010to2019)

all.equal(census_2010to2018, subset_2010to2019, tolerance = 0.5)
all.equal(census_2010to2018, subset_2010to2019, tolerance = 0.05)

## For the most part, it appears that differences are within a reasonable error
## to assume they are similar. Some correspondences are stronger than others.
## Calculated values seem to have the most error, but this could be because
## their algorithm adjusts each year vintages are calculate.
## 
## Therefore, we feel confident proceeding with the the 2010 to 2019 data set 
## in place of the 2010 to 2018 one.




## ----------------------------------------------------------------------------
## COMBINE THE 2010 to 2019 AND 2020 to 2023 DATASET

## Double check the nrow are the same as census_2010to2019
dim(census_2020to2023)


## Using the reference names, we will build out the combined data set. The
## goal is to organize the information the same way as the raw data sets.

## Recall
use_names

## The following for loop series will combine the two data sets by column name
## then date so that the columns are organized in the same manner as the 
## raw formats.

# Start with the metadata columns.
build <- cbind(census_2010to2019[, c(1:3)], census_2020to2023[, 2:3])
for (i in 1:length(use_names)) {
  
  for (j in 1:length(dates_included)) {
    # If the dates are between 2010 and 2019, then build from the
    # census_2010to2019 data.
    if (dates_included[j] %in% c(dates_included[1:10])) {
      data = census_2010to2019
      
      # If the dates are between 2020 and 2023, then build from the
      # census_2020to2023 data.
    } else if (dates_included[j] %in% c(dates_included[11:14])) {
      data = census_2020to2023
    }
    
    # Find the column index in the data that matches the current column name
    # with current the date included.
    index <- str_detect(colnames(data), str_c("\\b", use_names[i], "_", dates_included[j]))
    # Add this column to the building data set.
    build <- cbind(build, data[, index, drop = FALSE])
  }
  
}

# Commit the changes.
census_2010to2023 <- build




## ----------------------------------------------------------------------------
## FINAL CHECK FOR OBVIOUS ERRORS WITH HARMONIZATION
new_2010to2023 <- census_2010to2023
raw_2010to2019 <- census_2010to2019_raw
raw_2020to2023 <- census_2020to2023_raw


## The United States row was calculated for some dates above. We'll compare
## the state-level data only. Each of these is already organized alphabetically,
## and so no additional column ordering is required.

new_2010to2023 <- new_2010to2023 %>% filter(State %in% us_states[-c(1, 53)])
raw_2010to2019 <- raw_2010to2019 %>% filter(CTYNAME %in% us_states[-c(1, 53)])
raw_2020to2023 <- raw_2020to2023 %>% filter(Area_Name %in% us_states[-c(1, 53)])


## The unedited version of the 2010 to 2019 and 2020 to 2023 data sets include
## a "raw_" at the begging of their names. Each column name has been modified 
## since these were imported. If we strip differences by removing all 
## non-alphanumeric characters and converting to lower case, we can see if
## there are any not-easily reconciled discrepancies.

colnames(new_2010to2023) <- colnames(new_2010to2023) %>% str_replace_all(., "[^[:alnum:]]", "") %>% str_to_lower()
colnames(raw_2010to2019) <- colnames(raw_2010to2019) %>% str_replace_all(., "[^[:alnum:]]", "") %>% str_to_lower()
colnames(raw_2020to2023) <- colnames(raw_2020to2023) %>% str_replace_all(., "[^[:alnum:]]", "") %>% str_to_lower()


## As expected from previous results, it appears that column names associated
## with natural change are not included in either the 2010 to 2019 or
## 2020 to 2023 raw data sets. They are alternatively labeled as naturalinc.
a <- colnames(new_2010to2023)
b <- colnames(raw_2010to2019)
c <- colnames(raw_2020to2023)

a[a %!in% b & a %!in% c]

b[b %!in% a]
c[c %!in% a]


## Not considering the preceding metadata columns, which were removed early on
## in the process, it appears the principal difference is with the 2010 to 2019
## data set column names for natural change. We'll replace those so that they 
## are the same for our column-by-column comparison.

colnames(raw_2010to2019)[str_detect(colnames(raw_2010to2019), "natural")] <-
  # Find column names that include the string "natural".
  colnames(raw_2010to2019)[str_detect(colnames(raw_2010to2019), "natural")] %>%
  # Replace "inc" in that column name with "chg".
  str_replace(., "inc", "chg")


## We also need to filter out the duplicate "District of Columbia" rows.
raw_2010to2019 <- raw_2010to2019[-9, ] %>% `rownames<-`(NULL)
raw_2020to2023 <- raw_2020to2023[-9, ] %>% `rownames<-`(NULL)

## Adjust the the specific column name for the 2010 Census Population.
colnames(new_2010to2023)[colnames(new_2010to2023) == "censuspop2010"] <- "census2010pop"
colnames(new_2010to2023)[colnames(new_2010to2023) == "censuspop2020"] <- "census2020pop"

## In the column-by-column comparison, column names are assumed to be ordered
## the same way and that there are no extraneous columns. We see from above 
## that some metadata columns include dates, and so we'll exclude those to
## prevent a problem in our test.

# Store the identified columns that could disrupt the column-by-column test.
problem_columns <- c[c %!in% a][3:6]
# Remove these columns from the raw data set.
raw_2020to2023  <- raw_2020to2023[, colnames(raw_2020to2023) %!in% problem_columns]


## Now we are ready to do the column-by-column comparison.

result <- list()
# For loop for the dates 2010 to 2019.
for (i in 1:10) {
  # Subset the respective data set based on the detection of the dates in the
  # column name.
  reference <- raw_2010to2019[, str_detect(colnames(raw_2010to2019), dates_included[i])]
  query     <- new_2010to2023[, str_detect(colnames(new_2010to2023), dates_included[i])]
  
  # Check for differences between the two data sets.
  result[[i]] <- all.equal(reference, query, tolerance = 1e-3)
}

test_result <- do.call(rbind, result) %>% as.data.frame()


result <- list()
# Repeat the same loop for dates 2020 to 2023.
for (i in 11:14) {
  reference <- raw_2020to2023[, str_detect(colnames(raw_2020to2023), dates_included[i])]
  query     <- new_2010to2023[, str_detect(colnames(new_2010to2023), dates_included[i])]
  
  result[[i]] <- all.equal(reference, query, tolerance = 1e-3)
}

# Combine the results (list format) by adding rows.
test_result <- do.call(rbind, result[11:14]) %>% as.data.frame() %>% rbind(test_result, .) %>%
  # Rename the rows and column.
  `rownames<-`(c(dates_included)) %>% `colnames<-`(c("Corresponding Columns"))

test_result


## We see that all subsection of columns corresponding to the dates are
## identical (tolerance = 1e-3), with no discrepancy detected. Thus we feel
## confident proceeding with saving the harmonized census data.




## ----------------------------------------------------------------------------
## WRITING THE COMBINED FILE

write.csv(census_2010to2023, "Population Estimates and Projections/US_Census Population Estimates_2010 to 2023.csv")







