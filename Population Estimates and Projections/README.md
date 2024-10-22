# U.S. POPULATION ESTIMATES
## Sources and harmonization methods

The Johns Hopkins Coronovirus Resource Center (JHU CRC) GitHub page ([GovEX - US Country Map](https://github.com/govex/COVID-19/tree/master/data_tables/Data_for_UScounty_map)) utilizes the [U.S. Department of Agriculture Economic Research Services](https://www.ers.usda.gov/) Data Prouducts site for population estimations of intercensal data at the state level since the 2010 U.S. Census ([County-level Data Sets](https://www.ers.usda.gov/data-products/county-level-data-sets/county-level-data-sets-download-data/)).

The "U.S. Department of Agriculture_Population Estimates 2010 to 2018_JHU CRC.xls" data set includes census data from 2010 (captured 2010 Census value and estimated projection for 2010 using the current data and methods) with projections up through 2018 for U.S. states, districts, and state counties. Of the U.S. territories, only Puerto Rico is included. A complete set up through 2019 can be found directly on the [U.S. Census Bureau](https://www.census.gov/en.html) site (County Population Totals: 2010-2019 link below). 

Each year the U.S. Census Bereau releases additional projections, where all estimates are revised from April 1st of the census year through July 1st of the current year. This new vintage of estimates draws from the most current input data and employes methodological improvements that make these estimates superior to prior releases (U.S. Census Bereau's [Methods for County Population Totals: 2010-2019](https://www2.census.gov/programs-surveys/popest/technical-documentation/methodology/2010-2019/natstcopr-methv2.pdf)).

A projection of census values from the 2020 Census through 2023 can be found on either the U.S. Census Bureau Dataset webpage or on the U.S. Department of Agriculture's County-level Data Sets webpage. In this repository, this dataset was downloaded from the latter source, but both sources are expected to report the same information (County-level Data Sets link above).

While the original census dataset from JHU CRC's GovEX GitHub is included here, only the complete intercensal data from 2010-2019 sourced from the U.S. Census Bureau and the projection from the 2020 Census up to 2023 from the U.S. Department of Agriculture were harmonized and used in our analysis script.

Please see the "Population Data Cleaning Script.R" comments for further details describing the harmonization steps taken and accuracy checks completed to ensure that data integrety is maintained. Further description of intercensal and projection calculations/data handling can be found:
- U.S. Census Bureau: [County Population Totals: 2010-2019](https://www.census.gov/data/datasets/time-series/demo/popest/2010s-counties-total.html#par_textimage_70769902)
- U.S. Department of Agriculture: [County-level Data Sets Documentation](https://www.ers.usda.gov/data-products/county-level-data-sets/documentation/)

