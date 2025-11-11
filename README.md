# Repository for Practicing Collaborative Projects

## About The Coffee, Cookie and Coding $\left(C^3\right)$ Workshops

Yale's [Public Health Data Science and Data Equity (DSDE)](https://ysph.yale.edu/research/research-centers-and-initiatives/public-health-data-science-and-data-equity/) team hosts workshops, tutorials, and information sessions known as Coffee, Cookie and Coding ($C^3$) Workshops. These sessions are designed to help Public Health and Biostatistics masters-level students at Yale effectively leverage computational tools and analytical methods in their educational and professional endeavors. While primarily intended for the Yale community, all are welcome and encouraged to attend and benefit from our offerings.

You can find out more about past and upcoming work on our [website](https://ysph.yale.edu/public-health-research-and-practice/research-centers-and-initiatives/public-health-data-science-and-data-equity/events/). Additional tutorials will soon be available on our YouTube channel. If you are affiliated with Yale, you can set up an office hour appointment with one of our data scientists on our [Bookings Page](https://outlook.office365.com/book/DataScienceDataEquityOfficeHours@yale.edu/?ismsaljsauthenabled=true). The workshop content, including slides, handouts, and adaptations for asynchronous learning, is available on our [Book of Workshops](https://ysph-dsde.github.io/Book-of-Workshops/Git-and-GitHub/) webpage.

## About Workshop

**Workshop Title:** Getting Started with Git and GitHub Part 2

**Part 2 Instructors:**
- [Shelby Golden, M.S.](https://www.linkedin.com/in/shelby-golden/), Data Scientist I
- [Howard Baik, M.S.](https://www.linkedin.com/in/howard-baik/), Former Data Scientist I at Yale School of Public Health

Upon completing the workshop, you will be able to:
- Understand the purpose and value of Git and GitHub in managing coding projects.
- Get hands-on experience using Git and GitHub for solo projects through a walked-through example showing common workflows.
- Learn how to use GitHub to support collaboration and teamwork on group projects.

## Overview Of Contents

- **For the analysis:** `Plot Cases and Deaths.R`
- **R:** version 4.4.3
- **RStudio IDE:** version 2024.12.1+563
- ``renv`` is included to reproduce the environment.

## Using this Repository

This repository was created with pre-prepared code for students to practice version control using Git and GitHub for solo projects with real-world public health data. We ask that you create a "clean-break" copy of the repository into your own GitHub before cloning. You can find directions on how to do this on this workshop's [Book of Workshops](https://ysph-dsde.github.io/Book-of-Workshops/Git-and-GitHub) webpage, under the sections **Accessing the Codespaces** and **Making a Clean-Break Copy**.

Please note that all materials provided in this workshop, including any code added to your personal repository, belongs to DSDE. When using or referencing this material, please ensure to cite it correctly to give proper credit to the original authors.

## About the Data

The [Johns Hopkins Coronavirus Resource Center](https://coronavirus.jhu.edu/) (JHU CRC) tracked and compiled global COVID-19 pandemic data from January 22, 2020, to March 10, 2023. This data is publicly available through their two GitHub repositories. For this workshop content, we imported cumulative case and death counts for the U.S. from their [CSSEGISandData](https://github.com/CSSEGISandData/COVID-19) GitHub repository. The raw data for these two datasets used in the analysis can be found in the `csse_covid_19_data/csse_covid_19_time_series` subdirectory ([original source](https://github.com/CSSEGISandData/COVID-19/tree/master/csse_covid_19_data/csse_covid_19_time_series)). Both `time_series_covid19_confirmed_US.csv` and `time_series_covid19_deaths_US.csv were used`.

The data dictionaries provided by JHU CRC can be found here: [Cases and Deaths Datasets Data Dictionary](https://github.com/CSSEGISandData/COVID-19/tree/master/csse_covid_19_data#usa-daily-state-reports-csse_covid_19_daily_reports_us). For our purposes, we conducted data cleaning, harmonization, and smoothing using isotonic regression. This included harmonizing the U.S. Census Bureau's 2010 to 2019 population projections with the 2020 to 2023 vintages.

Details about these steps can be found in the `Git-and-GitHub/R` directory of this workshop’s GitHub repository ([link to code](https://github.com/ysph-dsde/Book-of-Workshops/tree/main/Git-and-GitHub/R)). The cleaned datasets used in this workshop can be found in the `Git-and-GitHub/Data` directory of this workshop’s GitHub repository ([link to data](https://github.com/ysph-dsde/Book-of-Workshops/tree/main/Git-and-GitHub/Data)).

## References

1. Dr. B. Moss, Dr. C. Watson, Dr. L. Rutkow, Dr. B. Garibaldi, B. Blauer, and Dr. L. Gardner, “Johns Hopkins Coronavirus Resource Center.” Accessed: Oct. 14, 2024. [Online]. Available: https://coronavirus.jhu.edu/

2. Johns Hopkins University Coronavirus Resource Center, “CSSEGISandData,” Center for Systems Science and Engineering (CSSE). Accessed: Nov. 02, 2024. [Online]. Available: https://github.com/CSSEGISandData/COVID-19

3. Johns Hopkins University Coronavirus Resource Center, “CSSEGISandData Data Dictionary,” Center for Systems Science and Engineering (CSSE). Accessed: Nov. 09, 2025. [Online]. Available: https://github.com/CSSEGISandData/COVID-19/tree/master/csse_covid_19_data#usa-daily-state-reports-csse_covid_19_daily_reports_us
  
4. Johns Hopkins University Coronavirus Resource Center, “Time Series COVID-19 Cases and Deaths US,” Center for Systems Science and Engineering (CSSE). Accessed: Nov. 09, 2025. [Online]. Available: https://github.com/CSSEGISandData/COVID-19/tree/master/csse_covid_19_data/csse_covid_19_time_series

5. Shelby Golden and Howard Baik, “Getting Started with Git and GitHub,” Book of Workshops. Accessed: Nov. 06, 2025. [Online]. Available: https://ysph-dsde.github.io/Book-of-Workshops/Git-and-GitHub/
