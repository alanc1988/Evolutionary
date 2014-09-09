getStats2 <- function()
{
  Statistics <- read.csv("Stats1.csv")
  
  Stats2 <- data.table(Statistics$Date)
  setnames(Stats2,"V1","Date")
  
  Stats2$Close <- Statistics$Close
  
  Stats2$PM1 <- Statistics$X1DayPercentMove
  Stats2$PM1_Outliers <- getOutliers(Stats2$PM1)
  Stats2$PM1_Streak <- getStreak(Stats2$PM1)
  Stats2$PM1_StreakPt25 <- getStreak(Stats2$PM1,absValue=.25)
  Stats2$PM1_StreakPt50 <- getStreak(Stats2$PM1,absValue=.5)
  Stats2$PM1_StreakP1 <- getStreak(Stats2$PM1,absValue=1)
  Stats2$PM1_StreakN1 <- getStreak(Stats2$PM1,absValue=1,positive=FALSE)
  
  Stats2$PM2 <- Statistics$X2DayPercentMove
  Stats2$PM2_Outliers <- getOutliers(Stats2$PM1)
  Stats2$PM2_Streak <- getStreak(Stats2$PM2)
  Stats2$PM2_StreakPt25 <- getStreak(Stats2$PM2,absValue=.25)
  Stats2$PM2_StreakPt50 <- getStreak(Stats2$PM2,absValue=.5)
  Stats2$PM2_StreakP1 <- getStreak(Stats2$PM2,absValue=1)
  Stats2$PM2_StreakN1 <- getStreak(Stats2$PM2,absValue=1,positive=FALSE)
  
  
  Stats2$PM3 <- Statistics$X3DayPercentMove
  Stats2$PM3_Outliers <- getOutliers(Stats2$PM3)
  Stats2$PM3_Streak <- getStreak(Stats2$PM3)
  Stats2$PM3_StreakPt25 <- getStreak(Stats2$PM3,absValue=.25)
  Stats2$PM3_StreakPt50 <- getStreak(Stats2$PM3,absValue=.5)
  Stats2$PM3_StreakP1 <- getStreak(Stats2$PM3,absValue=1)
  Stats2$PM3_StreakN1 <- getStreak(Stats2$PM3,absValue=1,positive=FALSE)
  
  
  Stats2$PM5 <- Statistics$X5DayPercentMove
  Stats2$PM5_Outliers <- getOutliers(Stats2$PM5)
  Stats2$PM5_Streak <- getStreak(Stats2$PM5)
  Stats2$PM5_StreakPt25 <- getStreak(Stats2$PM5,absValue=.25)
  Stats2$PM5_StreakPt50 <- getStreak(Stats2$PM5,absValue=.5)
  Stats2$PM5_StreakP1 <- getStreak(Stats2$PM5,absValue=1)
  Stats2$PM5_StreakN1 <- getStreak(Stats2$PM5,absValue=1,positive=FALSE)
  
  Stats2$PM10 <- Statistics$X10DayPercentMove
  Stats2$PM10_Outliers <- getOutliers(Stats2$PM10)
  Stats2$PM10_Streak <- getStreak(Stats2$PM10)
  Stats2$PM10_StreakPt25 <- getStreak(Stats2$PM10,absValue=.25)
  Stats2$PM10_StreakPt50 <- getStreak(Stats2$PM10,absValue=.5)
  Stats2$PM10_StreakP1 <- getStreak(Stats2$PM10,absValue=1)
  Stats2$PM10_StreakN1 <- getStreak(Stats2$PM10,absValue=1,positive=FALSE)
  
  Stats2$PM20 <- Statistics$X20DayPercentMove
  Stats2$PM20_Outliers <- getOutliers(Stats2$PM20)
  Stats2$PM20_Streak <- getStreak(Stats2$PM20)
  Stats2$PM20_StreakPt25 <- getStreak(Stats2$PM20,absValue=.25)
  Stats2$PM20_StreakPt50 <- getStreak(Stats2$PM20,absValue=.5)
  Stats2$PM20_StreakP1 <- getStreak(Stats2$PM20,absValue=1)
  Stats2$PM20_StreakN1 <- getStreak(Stats2$PM20,absValue=1,positive=FALSE)
  
  Stats2$PM50 <- Statistics$X50DayPercentMove
  Stats2$PM50_Outliers <- getOutliers(Stats2$PM50)
  Stats2$PM50_Streak <- getStreak(Stats2$PM50)
  Stats2$PM50_StreakPt25 <- getStreak(Stats2$PM50,absValue=.25)
  Stats2$PM50_StreakPt50 <- getStreak(Stats2$PM50,absValue=.5)
  Stats2$PM50_StreakP1 <- getStreak(Stats2$PM50,absValue=1)
  Stats2$PM50_StreakN1 <- getStreak(Stats2$PM50,absValue=1,positive=FALSE)
  
  Stats2$PM100 <- Statistics$X100DayPercentMove
  Stats2$PM100_Outliers <- getOutliers(Stats2$PM100)
  Stats2$PM100_Streak <- getStreak(Stats2$PM100)
  Stats2$PM100_StreakPt25 <- getStreak(Stats2$PM100,absValue=.25)
  Stats2$PM100_StreakPt50 <- getStreak(Stats2$PM100,absValue=.5)
  Stats2$PM100_StreakP1 <- getStreak(Stats2$PM100,absValue=1)
  Stats2$PM100_StreakN1 <- getStreak(Stats2$PM100,absValue=1,positive=FALSE)
  
  Stats2$PM200 <- Statistics$X200DayPercentMove
  Stats2$PM200_Outliers <- getOutliers(Stats2$PM200)
  Stats2$PM200_Streak <- getStreak(Stats2$PM200)
  Stats2$PM200_StreakPt25 <- getStreak(Stats2$PM200,absValue=.25)
  Stats2$PM200_StreakPt50 <- getStreak(Stats2$PM200,absValue=.5)
  Stats2$PM200_StreakP1 <- getStreak(Stats2$PM200,absValue=1)
  Stats2$PM200_StreakN1 <- getStreak(Stats2$PM200,absValue=1,positive=FALSE)
  
  #
  #
  #
  Stats2$DMA20 <- Statistics$X20DayDMA
  Stats2$DMA20_Outliers <- getOutliers(Stats2$DMA20)
  Stats2$DMA20_Streak <- getStreak(Stats2$DMA20)
  Stats2$DMA20_StreakPt25 <- getStreak(Stats2$DMA20,absValue=.25)
  Stats2$DMA20_StreakPt50 <- getStreak(Stats2$DMA20,absValue=.5)
  Stats2$DMA20_StreakP1 <- getStreak(Stats2$DMA20,absValue=1)
  Stats2$DMA20_StreakN1 <- getStreak(Stats2$DMA20,absValue=1,positive=FALSE)
  
  Stats2$DMA50 <- Statistics$X50DayDMA
  Stats2$DMA50_Outliers <- getOutliers(Stats2$DMA50)
  Stats2$DMA50_Streak <- getStreak(Stats2$DMA50)
  Stats2$DMA50_StreakPt25 <- getStreak(Stats2$DMA50,absValue=.25)
  Stats2$DMA50_StreakPt50 <- getStreak(Stats2$DMA50,absValue=.5)
  Stats2$DMA50_StreakP1 <- getStreak(Stats2$DMA50,absValue=1)
  Stats2$DMA50_StreakN1 <- getStreak(Stats2$DMA50,absValue=1,positive=FALSE)
  
  Stats2$DMA100 <- Statistics$X100DayDMA
  Stats2$DMA100_Outliers <- getOutliers(Stats2$DMA100)
  Stats2$DMA100_Streak <- getStreak(Stats2$DMA100)
  Stats2$DMA100_StreakPt25 <- getStreak(Stats2$DMA100,absValue=.25)
  Stats2$DMA100_StreakPt50 <- getStreak(Stats2$DMA100,absValue=.5)
  Stats2$DMA100_StreakP1 <- getStreak(Stats2$DMA100,absValue=1)
  Stats2$DMA100_StreakN1 <- getStreak(Stats2$DMA100,absValue=1,positive=FALSE)
  
  write.csv(Stats2, "Stats2.csv",row.names=FALSE)
}