library(data.table)
library(WriteXLS)
library(gdata)
#                 
#         IMPORTANT | IMPORTANT
#         IMPORTANT | IMPORTANT
#         IMPORTANT v IMPORTANT       Need to put your own directory path
#
AKDirectory <- "/Users/Robert/Desktop/Analytical_Kinetics"
#
#         IMPORTANT ^ IMPORTANT       Need to put your own directory path
#         IMPORTANT | IMPORTANT
#         IMPORTANT | IMPORTANT

setwd(AKDirectory)
source(paste(AKDirectory,"/importData.R",sep=""))
source(paste(AKDirectory,"/AK_Stats.R",sep=""))
source(paste(AKDirectory,"/getStats1.R",sep=""))
source(paste(AKDirectory,"/getStats2.R",sep=""))
source(paste(AKDirectory,"/Optimizer.R",sep=""))
source(paste(AKDirectory,"/getRelationships.R",sep=""))
source(paste(AKDirectory,"/VIX_Stats.R",sep=""))


# Downloads the data from Quandl and gets rid of known bad data
Data <- getData()

# Calculates Percent Move, Average True Range,
#   Autocor, Mean Abolute Deviation, Difference from the Moving Average etc 
#   Exports to a csv file named stats1.csv
getStats1() #About 5 Mins

# Calculates the running streaks and identifies outliers for Percent Moves and 
#     DMA's, using data from stats1. Exports to csv file named stats2.csv
getStats2()

Stats1 <- read.csv("Stats1.csv")
Stats2 <- read.csv("Stats2.csv")


getRelationships()
relationshipsPM <- read.csv("relationshipsPM.csv")

#
#
#

VIX <- getData(mode=2)


#Times to Revert from High to Normal
reversionTimes <- getReversionTimes(20,normal=c(-5:-1))
#Time to Hit x, given that it has been 20% below 20 for the last 20 days.
hitTimes <- getHitTimes(days = c(20,25,30))
#
VIX_Cors <- getVIX_Cors()
VIX1 <- data.frame(reversionTimes,hitTimes)

#write.csv(data.frame(reversionTimes,hitTimes), "VIX1.csv",row.names=FALSE)
#write.csv(t(VIX_Cors), "VIX_Cors.csv",row.names=names(VIX_Cors))

WriteXLS(c("Stats1","Stats2","relationshipsPM","VIX1","VIX_Cors"), ExcelFileName = "AKStats.xls", SheetNames = NULL)

