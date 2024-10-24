# Coffee, Cookie and Coding $\left(C^3\right)$

From Yale's Public Health Data Science and Data Equity (DSDE) Team

Workshop: Getting Started with Git and GitHub

Platform: Hybrid

Date: November $4^{\text{th}}$, 2024

## About The Workshop

These workshops are created by the DSDE data science team to assist Public Health and Biostatistics masters-level students effectively leverage computational tools and analytical methods in their educational and future professional endeavors.

We will be launching our Coffee, Cookie and Coding $\left(C^3\right)$ workshops with a starter guide on Git and GitHub. Upon completing the workshop, you will be able to: 

- Understand why it is good to learn to use Git and GitHub. 
- Configure your local Git and personal GitHub accounts and set up security keys. 
- Learn how to share and contribute to projects with an example created in RStudio.

The live workshop is not recorded, however, videos that complement the live session are posted on our groups YouTube (link) page and website (link). These cover the same material as was done in the live workshop.

Please find out more about our group on our lab website (Mukherjee lab website link). You can reach us \_\_\_.

## About This Repository

This GitHub repository is one of two that was used in the workshop. The other repository can be found at [JHU-CRC-Vaccinations](https://github.com/ysph-dsde/JHU-CRC-Vaccinations).

Enclosed is an analysis R script. Users of this repository will only need to open the `Analysis Script_Cases and Recoveries Plot.R`.

This project uses a `renv` lock so that the enviroment is reporducible for all users. Here we used R version 4.4.1. Users not using this version of R may experience problems running the script, even with the `renv` activated. After initially establishing your local copy (mirror) from this remote repository, activate the enviroment by running `renv::restore()` in the R Console and following the prompts.

NOTE: You may need to run `renv::restore()` twice to initialize and install all the packages indicated for by the lockfile. You know you are all set to go when running `renv::restore()` gives you `- The library is already synchronized with the lockfile.`

## About The Data Used

The [Johns Hopkins Coronavirus Resource Center](https://coronavirus.jhu.edu/) (JHU CRC) tracked and made publicly available COVID-19 pandemic data from January 22, 2020 to March 9, 2023. Here, we imported cumulative COVID-19 cases and death counts for the U.S. from their [CSSEGISandData/COVID-19](https://github.com/CSSEGISandData/COVID-19) GitHub repository. 