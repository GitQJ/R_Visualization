#Q1 - Read the file (using xlsx or readxl)
# install.packages("xlsx")  # Install xlsx package
library(xlsx) # load xlsx pacakge



# Set the working directory to where file is contained

sheet1 =read.xlsx("STAT8010-CA1_2024.xlsx",1) # Assign each sheed to a Data frame
sheet2 =read.xlsx("STAT8010-CA1_2024.xlsx",2)
sheet3 =read.xlsx("STAT8010-CA1_2024.xlsx",3)

#Q2 - Generate a data frame or tibble for each sheet in the file and correctly format each variable as appropriate.

df1 = sheet1 # Assign each reference data frame to one we can manipulate to keep original data in state
df2 = sheet2
df3 = sheet3

# Define data types for each columns

#install.packages("dplyr")
library(dplyr)

df1$BatchID = as.numeric(df1$BatchID)
df1$ProductName_Code = as.character(df1$ProductName_Code)
df1$SiteCode = as.numeric(df1$SiteCode)
df1$DateProduced = as.Date(df1$DateProduced)
df1$Volume = as.numeric(df1$Volume)
df1$BatchCost = as.numeric(df1$BatchCost)
df1$BaseBuffer = as.factor(df1$BaseBuffer)
df1$FeedType = as.factor(df1$FeedType)
df1$FinalConc = as.numeric(df1$FinalConc)
df1$FinalPH = as.numeric(df1$FinalPH)

df2$BatchID = as.numeric(df2$BatchID)
df2$ProdName = as.character(df2$ProdName)
df2$Site = as.numeric(df2$Site)
df2$Date_Produced = as.Date(df2$Date_Produced)
df2$Volume = as.numeric(df2$Volume)
df2$BatchCost = as.character(df2$BatchCost) #### Contains data error (addressing it in Q3),  cannot format as numeric yet
df2$BaseBuffer = as.factor(df2$BaseBuffer)
df2$FeedType = as.factor(df2$FeedType)
df2$ProdFinalConc = as.numeric(df2$ProdFinalConc)
df2$PH_Final = as.numeric(df2$PH_Final)

# Relocate columns of df2 to match df1
df2 = relocate(df2, Date_Produced, .after=Site)
df2 = df2 %>% relocate(Volume, .after = Date_Produced)
df2 = df2 %>% relocate(FeedType, .after = BaseBuffer)

#Rename columns of df2 to match df1

df2 = df2 %>% rename(ProductName_Code = ProdName,
                     SiteCode = Site,
                     DateProduced= Date_Produced,
                     FinalConc = ProdFinalConc,
                     FinalPH = PH_Final)

df3$BatchID = as.numeric(df3$BatchID)
df3$ProductName_Code = as.character(df3$ProductName_Code)
df3$SiteCode = as.numeric(df3$SiteCode)
df3$HoseTypeUsed = as.factor(df3$HoseTypeUsed)

# Checking variable formats

str(df1)
str(df2)
str(df3)

#Q3 - Amend errors on sheet 2 and make it compatible with sheet one

df2$BatchCost = gsub("b","6", df2$BatchCost) # Replace b with 6 for each values in the column
df2$BatchCost = gsub("O","0", df2$BatchCost) # Replace O with 0 for each values in the column

df2$BatchCost = as.numeric(df2$BatchCost) #Formatting Batchcost column after correcting data, to num as in df1

str(df2) # checking if all format match

#Q4 - Merge sheet 1 and 2 (make sure columns are consistent)

dfMerged = rbind(df1, df2)

#Q5 - Make a new column and calculate the age of each batch in weeks

dfMerged$AgeInWeeks = as.integer((as.Date("2024-10-01")-dfMerged$DateProduced)/7)

#Q6 - Create a new column for reference codes 

# for each row index
for (i in seq(nrow(dfMerged))) {
  # store Batch ID as string of character
  BatchID = as.character(dfMerged$BatchID[i])
  # store first letter of Product code
  firstProdLetter = substr(dfMerged$ProductName_Code[i],1,1)
  # store last number of Site Code
  lastSiteNum = substr(as.character(dfMerged$SiteCode[i]),
                       nchar(as.character(dfMerged$SiteCode[i])),
                       nchar(as.character(dfMerged$SiteCode[i])))
  
  # Combine info stored matching requested pattern
  refCode = paste(BatchID,
                  "-",
                  firstProdLetter,
                  lastSiteNum,
                  sep = "")
  
  # Assign the value to the column at the matching index
  dfMerged$refCodes[i] = refCode
}

# Checking if length of code as same number of batchID
length(unique(dfMerged$refCodes))

# Checking for duplicate codes
unique(duplicated(dfMerged$refCodes))

#Q7 - Add a new column for categories

# for each row index
for (i in seq(nrow(dfMerged))) {
  # Calculte Cost per Volumen
  costPerVol = dfMerged$BatchCost[i]/dfMerged$Volume[i]
  # Create conditional statement based on values to determine correct category
  if (costPerVol < 300 ) {
    category = "High Profit"  
  } else if (costPerVol >= 300 && costPerVol < 800 ) {
    category = "Moderate Profit"
  } else if (costPerVol >= 800 && costPerVol < 1200 ) {
    category = "Low Profit"
  } else if (costPerVol >= 1200 && costPerVol < 1800 ) {
    category = "Break Even"
  } else if (costPerVol >= 1800 && costPerVol < 3500 ) {
    category = "Tolerable Loss"
  } else {
    category = "Unacceptable Loss"
  }
  
  # Assign category to row at correct index
  dfMerged$Category[i] = category
}

#Q8 - Cost/Volume

#install.packages("tidyverse")
library(tidyverse)

# Create a vector of categories
categories = unique(dfMerged$Category)

# Create a new Data frame with correct data types
dfdata = data.frame(Category=as.character(),
                    TotalNumBatch=as.numeric(),
                    FinalPHmin=as.numeric(),
                    FinalPHmax=as.numeric(),
                    FinalPHmean=as.numeric(),
                    FinalPHmedian=as.numeric(),
                    FinalPHSD=as.numeric(),
                    FinalConcmin=as.numeric(),
                    FinalConcmax=as.numeric(),
                    FinalConcmean=as.numeric(),
                    FinalConcmedian=as.numeric(),
                    FinalConcSD=as.numeric())


# Foe each category of Cost per Volume
for (category in categories) {
  
  # Filter the data per category
  data = filter(dfMerged, Category == category)
  
  # Assign the descriptive statistics of the category to next row
  dfdata[nrow(dfdata) + 1,] = c(category,
                                nrow(data),
                                min(data$FinalPH),
                                max(data$FinalPH),
                                mean(data$FinalPH),
                                median(data$FinalPH),
                                sd(data$FinalPH),
                                min(data$FinalConc),
                                max(data$FinalConc),
                                mean(data$FinalConc),
                                median(data$FinalConc),
                                sd(data$FinalConc))
}

# Ordering the levels of the Data Frame
dfdata$Category = factor(dfdata$Category, levels = c("High Profit" , "Moderate Profit", "Low Profit", "Break Even", "Tolerable Loss", "Unacceptable Loss" ))

# Ordering the Data Frame by levels
dfdata <- dfdata[order(dfdata$Category),]

# Barplot of feedtypes

dfMerged %>%
  ggplot(aes(x=Category)) +
  geom_bar(aes(fill = FeedType), position = position_dodge())

# Sorting categories in Profitability order
dfMerged$Category = factor(dfMerged$Category, levels = c("High Profit" , "Moderate Profit", "Low Profit", "Break Even", "Tolerable Loss", "Unacceptable Loss" ))

# Display plot
dfMerged %>%
  ggplot(aes(x=Category)) +
  geom_bar(aes(fill = FeedType), position = position_dodge()) +
  ggtitle("Feed Types per Category Bar plot") +
  scale_x_discrete(labels = c(
    "High Profit"   = "HP", 
    "Moderate Profit" = "MP", 
    "Low Profit" = "LP",
    "Break Even" = "BE",
    "Tolerable Loss" = "TL",
    "Unacceptable Loss" = "UL"))

# Boxplot of Conc
dfMerged %>%
  ggplot(aes(x=Category)) +
  ggtitle("Final Concentration per Category Types Box plot") +
  scale_x_discrete(labels = c(
    "High Profit"   = "HP", 
    "Moderate Profit" = "MP", 
    "Low Profit" = "LP",
    "Break Even" = "BE",
    "Tolerable Loss" = "TL",
    "Unacceptable Loss" = "UL")) +
  geom_boxplot(aes(y=FinalConc))

# Boxplot of PH
dfMerged %>%
  ggplot(aes(x=Category)) +
  ggtitle("Final PH per Category Types Box plot") +
  scale_x_discrete(labels = c(
    "High Profit"   = "HP", 
    "Moderate Profit" = "MP", 
    "Low Profit" = "LP",
    "Break Even" = "BE",
    "Tolerable Loss" = "TL",
    "Unacceptable Loss" = "UL")) +
  geom_boxplot(aes(y=FinalPH))


#Q9 - Scatterplot with Volume on the x-axis and BatchCost on the y-axis

dfMerged %>%
  # Define x and Y for plot
  ggplot(aes(x=Volume, y = BatchCost)) +
  # Define variable for size, shape and colour
  geom_point(aes(size = FinalConc, shape = FeedType, colour = Category))+
  ggtitle("Volume against BatchCost Scatter plot") +
  # Create dividing lines with a different color
  geom_segment(aes(x = 0, xend = 29, y=0,yend = 100000), colour = "magenta")+
  geom_segment(aes(x = 0, xend = 55, y=0,yend = 100000), colour = "blue")+
  geom_segment(aes(x = 0, xend = 83, y=0,yend = 100000), colour = "red")+
  geom_segment(aes(x = 0, xend = 95, y=0,yend = 75000), colour = "green")+
  geom_segment(aes(x = 0, xend = 90, y=0,yend = 27500), colour = "cyan")+
  geom_segment(aes(x = 0, xend = 110, y=0,yend = 10000), colour = "gold")
  

#Q10 - Merge Hose data from sheet3

# Filter data from 3rd Data Frame where BatchID is N/A
df3na = df3 %>%
  filter(,is.na(BatchID))

# Add those row under the rest of the data Frame
dfMerged = bind_rows(dfMerged,df3na)

# Filter data from 3rd Data Frame where BatchID is available
df3nona = df3 %>%
  filter(,!is.na(BatchID))


# For each Batch in this filter
for (Batch in df3nona$BatchID) {
  # Store the hose type
  hose = df3nona$HoseTypeUsed[df3nona$BatchID ==Batch]
  # Assign the hose type to the matching batch in the merged Data Frame
  dfMerged$HoseTypeUsed[dfMerged2$BatchID == Batch] = hose
}

#Q11 - Create a smaller dataframe where the Hose information is available

# Create a small data frame from the merged data frame
smalldf = dfMerged %>%
  # Filter where Hose Type is available
  filter(HoseTypeUsed != is.na(HoseTypeUsed)) %>%
  # Filter where Batch ID is available
  filter(BatchID != is.na(BatchID))

# Recalculate the Age in weeks for new values
smalldf$AgeInWeeks = as.integer((as.Date("2024-10-01")-smalldf$DateProduced)/7)

# Get a summary of the data frame
summary(smalldf)

#View new data Frame
view(smalldf)

# Exploring Visualizations (Present in report)

# Barplot of Hosetypes for categories

# Sorting categories in Profitability order
smalldf$Category = factor(smalldf$Category, levels = c("High Profit" , "Moderate Profit", "Low Profit", "Break Even", "Tolerable Loss", "Unacceptable Loss" ))

# Display plot
smalldf %>%
  ggplot(aes(x=HoseTypeUsed)) +
  geom_bar(aes(fill=Category), position = position_dodge()) +
  ggtitle("Hose Types per Category Bar plot")

# Sorting categories in Profitability order
dfMerged$Category = factor(dfMerged$Category, levels = c("High Profit" , "Moderate Profit", "Low Profit", "Break Even", "Tolerable Loss", "Unacceptable Loss" ))

# Display plot
dfMerged %>%
  ggplot(aes(x=Category)) +
  geom_bar(aes(fill = FeedType), position = position_dodge()) +
  ggtitle("Feed Types per Category Bar plot") +
  scale_x_discrete(labels = c(
    "High Profit"   = "HP", 
    "Moderate Profit" = "MP", 
    "Low Profit" = "LP",
    "Break Even" = "BE",
    "Tolerable Loss" = "TL",
    "Unacceptable Loss" = "UL"))

# Scatterplots of Conc against volume, per Hose types
smalldf %>%
  ggplot(aes(x=FinalConc, y=Volume)) +
  ggtitle("Final Concentration against Volume Scatter plot") +
  geom_point(aes(colour = HoseTypeUsed,size = 2))

# Scatterplots of PH against volume, per Hoset ypes
smalldf %>%
  ggplot(aes(x=FinalPH, y=Volume)) +
  ggtitle("Final PH against Volume Scatter plot") +
  geom_point(aes(colour = HoseTypeUsed,size = 2))

# Bar plots of Categories for hose types, per Feed types 
smalldf %>%
  ggplot(aes(x=Category)) +
  ggtitle("Hose Types per Category for Feed Types Bar plot") +
  geom_bar(aes(fill=HoseTypeUsed), position = position_dodge())+
  scale_x_discrete(labels = c(
    "High Profit"   = "HP", 
    "Moderate Profit" = "MP", 
    "Low Profit" = "LP",
    "Break Even" = "BE",
    "Tolerable Loss" = "TL",
    "Unacceptable Loss" = "UL")) +
  facet_grid(~FeedType)

# Bar plots of Categories for buffers, per Hose types 
smalldf %>%
  ggplot(aes(x=Category)) +
  ggtitle("Base Buffers per Category for Hose Types Bar plot") +
  geom_bar(aes(fill=BaseBuffer), position = position_dodge())+
  scale_x_discrete(labels = c(
    "High Profit"   = "HP", 
    "Moderate Profit" = "MP", 
    "Low Profit" = "LP",
    "Break Even" = "BE",
    "Tolerable Loss" = "TL",
    "Unacceptable Loss" = "UL")) +
  facet_grid(~HoseTypeUsed)

# Exploring Visualizations (NOT Present in report - Only used for investigating variables, not commenting)

dfMerged %>%
  ggplot(aes(x=Volume, y = FinalConc)) +
  ggtitle("Volume on Concentrations, per Buffer, Feed and PH Scatter plot (whole data)") +
  geom_point(aes(size = FinalPH, shape = BaseBuffer, colour = FeedType))

dfMerged %>%
  ggplot(aes(x=Category)) +
  ggtitle("Category on Volume for Feed Types Box plot whole data (whole data)") +
  scale_x_discrete(labels = c(
    "High Profit"   = "HP", 
    "Moderate Profit" = "MP", 
    "Low Profit" = "LP",
    "Break Even" = "BE",
    "Tolerable Loss" = "TL",
    "Unacceptable Loss" = "UL")) +
  geom_boxplot(aes(y=Volume, colour = FeedType))

dfMerged %>%
  ggplot(aes(x=Category)) +
  ggtitle("Category on Volume for Hose Types Box plot (whole data)") +
  scale_x_discrete(labels = c(
    "High Profit"   = "HP", 
    "Moderate Profit" = "MP", 
    "Low Profit" = "LP",
    "Break Even" = "BE",
    "Tolerable Loss" = "TL",
    "Unacceptable Loss" = "UL")) +
  geom_boxplot(aes(y=Volume, colour = HoseTypeUsed))

smalldf %>%
  ggplot(aes(x=FinalConc, y=Volume)) +
  ggtitle("Concentrations on Volume, per Feed Scatter plot (small data)") +
  geom_point(aes(colour = FeedType,size = 2))

smalldf %>%
  ggplot(aes(x=FinalConc, y=BatchCost)) +
    ggtitle("Concentrations on Volume Scatter, per Hose plot (small data)") +
  geom_point(aes(colour = HoseTypeUsed,size = 2))

smalldf %>%
  ggplot(aes(x=FinalPH, y=BatchCost)) +
  ggtitle("PHs on Volume Scatter plot, per Hose (small data)") +
  geom_point(aes(colour = HoseTypeUsed,size = 2))

smalldf %>%
  ggplot(aes(x=HoseTypeUsed)) +
  ggtitle("PHs on Volume Scatter plot, per Feed (small data)") +
  geom_bar(aes(fill=FeedType), position = position_dodge())

smalldf %>%
  ggplot(aes(x=Category)) +
  ggtitle("Category on Feed, per Hose Bar plot (small data)") +
  scale_x_discrete(labels = c(
    "High Profit"   = "HP", 
    "Moderate Profit" = "MP", 
    "Low Profit" = "LP",
    "Break Even" = "BE",
    "Tolerable Loss" = "TL",
    "Unacceptable Loss" = "UL")) +
  geom_bar(aes(fill=FeedType), position = position_dodge())+
  facet_grid(~HoseTypeUsed)

smalldf %>%
  ggplot(aes(x=HoseTypeUsed)) +
  ggtitle("Hose on Buffer, per Category Bar plot (small data)") +
  geom_bar(aes(fill=BaseBuffer), position = position_dodge())+
  scale_x_discrete(labels = c(
    "High Profit"   = "HP", 
    "Moderate Profit" = "MP", 
    "Low Profit" = "LP",
    "Break Even" = "BE",
    "Tolerable Loss" = "TL",
    "Unacceptable Loss" = "UL")) +
  facet_grid(~Category)

smalldf %>%
  ggplot(aes(x=Category)) +
  ggtitle("Category on Age, per Hose Box plot (small data)") +
  scale_x_discrete(labels = c(
    "High Profit"   = "HP", 
    "Moderate Profit" = "MP", 
    "Low Profit" = "LP",
    "Break Even" = "BE",
    "Tolerable Loss" = "TL",
    "Unacceptable Loss" = "UL")) +
  geom_boxplot(aes(y=AgeInWeeks, colour = HoseTypeUsed))

smalldf %>%
  ggplot(aes(x=AgeInWeeks, y=FinalPH)) +
  ggtitle("Age on PH, per Hose Scatter plot (small data)") +
  geom_point(aes(colour = HoseTypeUsed,size = 2))
