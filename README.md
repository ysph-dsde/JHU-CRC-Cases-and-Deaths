# Getting Started with Git and GitHub

## About The Coffee, Cookie and Coding $\left(C^3\right)$ Workshops

Yale's Public Health Data Science and Data Equity (DSDE) team created this workshop series for Public Health and Biostatistics masters-level students at Yale. They are designed to help learners effectively leverage computational tools and analytical methods in their educational and professional endeavors. You can find out more about past and upcoming tutorials on our YouTube (coming soon) and [website](https://ysph.yale.edu/public-health-research-and-practice/research-centers-and-initiatives/public-health-data-science-and-data-equity/events/).

You can find out more about past and upcoming tutorials on our YouTube (coming soon) and [website](https://ysph.yale.edu/public-health-research-and-practice/research-centers-and-initiatives/public-health-data-science-and-data-equity/events/). The workshop content (slides, asynchronous learning materials, and references) are available from our [Book of Workshops](https://ysph-dsde.github.io/Book-of-Workshops/Git-and-GitHub/) webpage.

If you are affiliated with Yale, you can set up an office hour appointment with one of the data scientists ([Bookings Page](https://outlook.office365.com/owa/calendar/DataScienceDataEquityOfficeHours@yale.edu/bookings/)).

## About Workshop

**Workshop Title:** &nbsp; Getting Started with Git and GitHub pt. 2

**Date:** &emsp;&emsp;&emsp;&emsp;&emsp;&nbsp; Monday April $28^{\text{th}}$, 2025

Upon completing the workshop, you will be able to:
- Understand why Git and GitHub are valuable tools for version control and managing coding projects.
- Get hands-on experience using Git and GitHub for solo projects through a worked through example showing common workflows.
- Learn how to use GitHub to support collaboration and teamwork on group projects.

## Overview Of Contents

- **For the analysis:** `Analysis Script_Cases and Deaths Plot.R`
- **R:** version 4.4.3
- **RStudio IDE:** version 2024.12.1+563
- ``renv`` is included to reproduce the environment.

## Using this Repository

We ask that you create a "clean-break" copy of the repository into your own GitHub prior to cloning. You can find directions on how to do this on the [Book of Workshops](https://ysph-dsde.github.io/Book-of-Workshops/Git-and-GitHub/#codespaces) webpage under the sections **Codespaces** and **Making a Clean-Break Copy**.

## About the Data

The [Johns Hopkins Coronavirus Resource Center](https://coronavirus.jhu.edu/) (JHU CRC) tracked and compiled global COVID-19 pandemic data from January 22, 2020 and March 10, 2023. These data are publically available through their two GitHub repositories. We imported two datasets for this workshop content:

- Cumulative vaccination counts for the U.S. from their [GovEX/COVID-19](https://github.com/govex/COVID-19/tree/master/data_tables/vaccine_data) GitHub repository. The raw data used in the analysis script can be found in the `data_tables/vaccine_data/us_data/time_series` subdirectory ([original source](https://github.com/govex/COVID-19/blob/master/data_tables/vaccine_data/us_data/time_series/time_series_covid19_vaccine_us.csv)).
- Cumulative case and death counts for the U.S. from their [CSSE GitHub](https://github.com/CSSEGISandData/COVID-19). The raw data for these two datasets used in the analysis can be found in the `csse_covid_19_data/csse_covid_19_time_series` subdirectory ([original source](https://github.com/CSSEGISandData/COVID-19/tree/master/csse_covid_19_data/csse_covid_19_time_series)). Both `time_series_covid19_confirmed_US.csv` and `time_series_covid19_deaths_US.csv` were used.

The data dictionaries provided by JHU CRC can be found here: [Vaccinations Dataset Data Dictionary](https://github.com/govex/COVID-19/tree/master/data_tables/vaccine_data/us_data) and [Cases and Deaths Datasets Data Dictionary](https://github.com/CSSEGISandData/COVID-19/tree/master/csse_covid_19_data#usa-daily-state-reports-csse_covid_19_daily_reports_us). For our purposes, we conducted some data cleaning, harmonization, and smoothing using an isotonic regression. This included harmonizing the U.S. Census Bureau's 2010 to 2019 population projections with 2020 to 2023 vintages. 

Details about these steps can be found in the `Git-and-GitHub/R` directory of this workshop's GitHub repository ([link to code](https://github.com/ysph-dsde/Book-of-Workshops/tree/main/Git-and-GitHub/R)). The cleaned datasets used in this workshop can be found in the `Git-and-GitHub/Data` directory of this workshop's GitHub repository ([link to data](https://github.com/ysph-dsde/Book-of-Workshops/tree/main/Git-and-GitHub/Data)).