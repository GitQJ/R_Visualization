# R Visualisation
Visualisation and Analysis of Batch production in R

# Summary
In this report, we are wrangling different data sets of batch production. Each batch is identified with unique numbers, Product and Site codes, labelled with their dates of productions, cost, volume, base buffer and feed type, outputs of PH and Concentration. We also have batches where we are provided the Hose type used for production. Our job here is to clean up the data, assess the statistics and provide visualizations to provide a report to our manager and identify patterns.

We are provided spreadsheets where the first 2 are containing information related to different batches. We can notice the formats for column names and order in table are different so some data cleaning steps are needed to combine those 2 spreadsheet to form a unique data set we can use The last spreadsheet contains information that would complete some entries and add the Hose Type used for some batches. Some additional data wrangling steps will be needed in order to merge such information to appropriate rows.

After that, we will provide some data visualisations and descriptive statistics and eventually draw relationships between variables. We will provide the descriptive statistics of Output concentration of batch product and Final PH measurement of batch output and see if there is a pattern with the Feed Types.

We will also create profitability categories, based on Cost per Volume and use visualization to see if a pattern exist with the Feed Types and Concentrations.

Finally, we will create a smaller data frame where we can Hose Type is available explore, filters, visualisations to provide any findings.

# Tools
- R
- dplyr
- ggplot2
