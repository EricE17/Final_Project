#Install packages
#package_vector = c("shiney","leaflet","tidyverse","magrittr")
#install.packages("shinythemes")

#Libraries
#library("cosinor2")
library("ggplot2")
library("dataRetrieval")
library("tidyr")
library("leaflet")
library("magrittr") 
library("shinythemes")

#Water gage location
gagedata <- readNWISdata(siteNumbers = "10168300", parameterCd = "00060", 
                         startDate = "1986-10-01", endDate = "2005-12-31")
names(gagedata)[3] <- "Date"
names(gagedata)[4] <- "Daily_Discharge"

#Brighton Snotel data
Brighton_snotel_data <- read.csv("SWE_equivalent_csv.csv")
Brighton_snotel_data$count <- 1:7032
gagedata$count <- 1:7032

#Merged data file and recolumn rename
merged <- merge.data.frame(Brighton_snotel_data, gagedata, by = "count")
names(merged)[2] <- "Date"
names(merged)[7] <- "Discharge"

merged

# convert date column to date class
merged$Date <- as.Date(merged$Date, format = "%Y-%m-%d",str(merged))

# create variables of the week and month of each observation:
merged$Month <- as.Date(cut(merged$Date,
                            breaks = "month"))



merged <- separate(data = merged,col = "Month",into = c("year","month","day"))


# check varaible types -- make sure Date(.x , .y) are listed as dates
str(merged)

# view R class of data 
class(merged$Date)

# view results
head(merged$Date)

# remove columns
merged$agency_cd <- NULL
merged$Date.y <- NULL
merged$X_00060_00003_cd <- NULL
merged$tz_cd <- NULL


#SWE difference between two adjoining events
merged$SWEdrop <- c(0,  diff(merged$SWE, lag = 1, differences = 1))

# removal of low SWE and discharge values (values below 2.0 have been removed)
# variable removal
mergedSWEdrop <- merged$SWEdrop
mergeddischarge <- merged$Discharge

# dataframe compilation
filteredmerged <- data.frame(mergedSWEdrop,mergeddischarge)

# new filtered data file
filteredmerged <- filteredmerged[ which(filteredmerged$mergedSWEdrop > -3 & filteredmerged$mergedSWEdrop < -0.1 & filteredmerged$mergeddischarge > 4 ),]

# absolute value of mergedSWEdrop
filteredmerged$mergedSWEdrop <- abs(filteredmerged$mergedSWEdrop)