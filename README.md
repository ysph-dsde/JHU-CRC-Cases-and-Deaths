# Getting Started with Git and GitHub

## About The Coffee, Cookie and Coding $\left(C^3\right)$ Workshops

Yale’s [Public Health Data Science and Data Equity (DSDE)](https://ysph.yale.edu/research/research-centers-and-initiatives/public-health-data-science-and-data-equity/) team hosts workshops, tutorials, and information sessions known as Coffee, Cookie and Coding $\left(C^3\right)$ Workshops. These sessions are designed to help Public Health and Biostatistics masters-level students at Yale effectively leverage computational tools and analytical methods in their educational and professional endeavors.

While primarily intended for the Yale community, all are welcome and encouraged to attend and benefit from our offerings. If you are affiliated with Yale, you can set up an office hour appointment with one of the data scientists ([Bookings Page](https://outlook.office365.com/owa/calendar/DataScienceDataEquityOfficeHours@yale.edu/bookings/)).

## About Workshop

**Workshop Title:** &nbsp; Getting Started with Git and GitHub Part 2

**Last Dated:** &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; February $11^{\text{th}}$, 2026

**Part 2 Instructors:**
- [Shelby Golden, M.S.](https://ysph.yale.edu/profile/shelby-golden/), Data Scientist I
- [Howard Baik, M.S.](https://www.linkedin.com/in/howard-baik/), Former Data Scientist I at Yale School of Public Health

Upon completing the workshop, you will be able to:

- Understand the purpose and value of Git and GitHub in managing coding projects.
- Master essential Git and GitHub workflows for individual projects through hands-on practice.
- Get introduced to collaborative workflows and teamwork strategies using guided GitHub examples.

## Overview Of Contents

- **For the analysis:** `Plot Cases and Deaths.R`
- **R:** version 4.5.2
- **RStudio IDE:** version 2026.01.0+392
- ``renv`` is included to reproduce the environment.

All session materials, interactive learning assignments, and codebase links are available in the [Book of Workshops](https://ysph-dsde.github.io/Book-of-Workshops/Workshops/Git-and-GitHub/git-github-index.html) chapter. Some of these materials have been adapted for asynchronous learning in webpage format.

## Using this Repository

This repository contains prepared code for students to practice version control using Git and GitHub with real-world public health data. Please create a "clean-break" of the repository in your own GitHub before cloning it. Directions are available on the [Accessing the Codespaces](https://ysph-dsde.github.io/Book-of-Workshops/Workshops/Git-and-GitHub/git-github-index.html#accessing-the-codespaces) section of this workshop's Book of Workshops webpage.

Please note that all slides, handouts, and code provided in this workshop, including any added to your personal repository, belongs to DSDE. When using or referencing this material, please ensure to cite it correctly to give proper credit to the original authors.

## About the Data

The [Johns Hopkins Coronavirus Resource Center](https://coronavirus.jhu.edu/) (JHU CRC) tracked and compiled global COVID-19 pandemic data from January 22, 2020, to March 10, 2023. This data is publicly available through their two GitHub repositories. For this workshop content, we imported two datasets:

- Cumulative vaccination counts for the U.S. from their [GovEX/COVID-19](https://github.com/govex/COVID-19) GitHub repository. The raw data used in the analysis script can be found in the `data_tables/vaccine_data/us_data/time_series` subdirectory ([original source](https://github.com/govex/COVID-19/blob/master/data_tables/vaccine_data/us_data/time_series/time_series_covid19_vaccine_us.csv)).

- Cumulative case and death counts for the U.S. from their [CSSEGISandData](https://github.com/CSSEGISandData/COVID-19) GitHub repository. The raw data for these two datasets used in the analysis can be found in the `csse_covid_19_data/csse_covid_19_time_series` subdirectory ([original source](https://github.com/CSSEGISandData/COVID-19/tree/master/csse_covid_19_data/csse_covid_19_time_series)). Both `time_series_covid19_confirmed_US.csv` and `time_series_covid19_deaths_US.csv were used`.

The data dictionaries provided by JHU CRC can be found here: [Vaccinations Dataset Data Dictionary](https://github.com/govex/COVID-19/tree/master/data_tables/vaccine_data/us_data) and [Cases and Deaths Datasets Data Dictionary](https://github.com/CSSEGISandData/COVID-19/tree/master/csse_covid_19_data#usa-daily-state-reports-csse_covid_19_daily_reports_us). For our purposes, we conducted data cleaning, harmonization, and smoothing using isotonic regression. This included harmonizing the U.S. Census Bureau's 2010 to 2019 population projections with the 2020 to 2023 vintages.

Details about these steps can be found in the `Git-and-GitHub/Code/Data Cleaning` and `Git-and-GitHub/Code/Population Estimates and Projections` directories ([link to code](https://github.com/ysph-dsde/Book-of-Workshops/tree/main/Workshops/Git-and-GitHub/Code)). The cleaned datasets are in the `Git-and-GitHub/Data` directory ([link to data](https://github.com/ysph-dsde/Book-of-Workshops/tree/main/Workshops/Git-and-GitHub/Data)).

## References

1. Dr. B. Moss, Dr. C. Watson, Dr. L. Rutkow, Dr. B. Garibaldi, B. Blauer, and Dr. L. Gardner, “Johns Hopkins Coronavirus Resource Center.” Accessed: Oct. 14, 2024. [Online]. Available: https://coronavirus.jhu.edu/

2. Johns Hopkins University Coronavirus Resource Center, “CSSEGISandData,” Center for Systems Science and Engineering (CSSE). Accessed: Nov. 02, 2024. [Online]. Available: https://github.com/CSSEGISandData/COVID-19

3. Johns Hopkins University Coronavirus Resource Center, “CSSEGISandData Data Dictionary,” Center for Systems Science and Engineering (CSSE). Accessed: Nov. 09, 2025. [Online]. Available: https://ysph-dsde.github.io/Book-of-Workshops/Workshops/Git-and-GitHub/git-github-index.html
  
4. Johns Hopkins University Coronavirus Resource Center, “Time Series COVID-19 Cases and Deaths US,” Center for Systems Science and Engineering (CSSE). Accessed: Nov. 09, 2025. [Online]. Available: https://github.com/CSSEGISandData/COVID-19/tree/master/csse_covid_19_data/csse_covid_19_time_series

5. Shelby Golden and Howard Baik, “Getting Started with Git and GitHub,” Book of Workshops. Accessed: Nov. 06, 2025. [Online]. Available: https://ysph-dsde.github.io/Book-of-Workshops/Workshops/Git-and-GitHub/git-github-index.html


