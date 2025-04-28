## ----------------------------------------------------------------------------
## From Yale's Public Health Data Science and Data Equity (DSDE) Team
##
## Workshop: Getting Started with Git and GitHub pt. 2
##  Authors: Shelby Golden, M.S.
##     Date: 2025-04-27
## 
##    R version: 4.4.3
## renv version: 1.0.11


## ----------------------------------------------------------------------------
## SET UP THE ENVIRONMENT
## renv() will install all of the packages and their correct version used here
renv::init()          # Initialize the project
renv::restore()       # Download packages and their version saved in the lockfile.

suppressPackageStartupMessages({
  library("readr")         # For reading in the data
  library("dplyr")         # For data manipulation
  library("ggplot2")       # For creating static visualizations
  library("lubridate")     # Facilitates working with dates and times
  library("scales")        # Override default ggplot2 axes and legend settings
})

# Function to select "Not In"
'%!in%' <- function(x,y)!('%in%'(x,y))




## ----------------------------------------------------------------------------
## LOAD IN THE DATA
## This data is from the COVID-19 Data Repository by the Center for Systems 
## Science and Engineering (CSSE) at Johns Hopkins University. We load it in
## directly from their GitHub page using the raw URL.

covid19_cases_deaths_url <- "https://raw.githubusercontent.com/ysph-dsde/Book-of-Workshops/refs/heads/main/Git-and-GitHub/Data/Deaths%20and%20Cases%20Aggregated%20by%20Week.csv"
covid19_cases_deaths     <- read_csv(file = covid19_cases_deaths_url, show_col_types = FALSE) |>
  as.data.frame()

# Inspect the data.
glimpse(covid19_cases_deaths)




## ----------------------------------------------------------------------------
## DATA VISUALIZATION

# The US states and territories represented.
covid19_cases_deaths$Province_State |> unique()

# The earliest and latest date represented. All weeks between these dates
# are included, and each region has the same span of dates.
covid19_cases_deaths$Week |> min()
covid19_cases_deaths$Week |> max()


# Plot 1: Cases
plot_cases <- covid19_cases_deaths |>
  # Select the national level data only.
  filter(Province_State == "United States") |>
  # Start ggplot with date on x-axis and daily count on y-axis
  ggplot(aes(Week, Confirmed_Cases_Daily)) +
    # Add a line plot with a specified color
    geom_line(color = "#00356b") +
    # Format x-axis dates as month/year
    scale_x_date(date_labels = "%m/%Y",
                 breaks = as.Date(c("2020-01-01", "2020-06-30",
                                    "2021-01-01", "2021-06-30",
                                    "2022-01-01", "2022-06-30",
                                    "2023-01-01", "2023-06-30"))) +
    # Format the y-axis to display counts with commas
    scale_y_continuous(labels = scales::label_comma()) +
    # Add labels and title to the plot
    labs(x = NULL, y = "Daily Counts", 
         title = "Daily Confirmed Counts of COVID-19 in the US") +
    # Apply a minimal theme to the plot for a clean appearance
    theme_minimal()



# Plot 2: Deaths
plot_deaths <- covid19_cases_deaths %>% 
  # Select the national level data only.
  filter(Province_State == "United States") |>
  # Start ggplot with date on x-axis and daily count on y-axis
  ggplot(aes(Week, Deaths_Daily)) +
    # Add a line plot with a specified color
    geom_line(color = "#A353FF") +
    # Format x-axis dates as month/year
    scale_x_date(date_labels = "%m/%Y",
                 breaks = as.Date(c("2020-01-01", "2020-06-30",
                                    "2021-01-01", "2021-06-30",
                                    "2022-01-01", "2022-06-30",
                                    "2023-01-01", "2023-06-30"))) +
    # Format the y-axis to display counts with commas
    scale_y_continuous(labels = scales::label_comma()) +
    # Add labels and title to the plot
    labs(x = NULL, y = "Daily Counts",
        title = "Daily Death Counts of COVID-19 in the US") +
    # Apply a minimal theme to the plot for a clean appearance
    theme_minimal()




## ----------------------------------------------------------------------------
## SAVE THE FILES

ggsave("plot_cases.jpeg", plot_cases, width = 20, height = 12, units = "cm")
ggsave("plot_deaths.jpeg", plot_deaths, width = 20, height = 12, units = "cm")



