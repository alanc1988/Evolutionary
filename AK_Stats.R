
# For every day in the data, returns the % the market has moved in the 
#   specified period.
getPercentMove <- function(period=1)
{
  PM <- c()
  for(day in 1:(nrow(Data)-period))
  {
    move <- (Data[day,Settle]-Data[day+period,Settle])/Data[day+period,Settle]
    move <- signif(move,5)*100
    PM <- rbind(PM,move)
  }
  for(day in 1:period)
  {
    PM <- rbind(PM,0.0)
  }
  signif(unname(PM),4)
}

# For every day, gets the Average True Range over specified days  
#   for 2 periods of data.
getATR <- function(days=2)
{
  period = 2
  ATR <- c()
  for(day in 1:(nrow(Data)-period*days))
  {
      high1 <- max(c(Data$High[day:(day+days-1)],Data$Settle[day:(day+days-1)]))
      low1 <- min(c(Data$Low[day:(day+days-1)],Data$Settle[day:(day+days-1)]))
      tr1 <- high1 - low1
      high2 <- max(c(Data$High[(day+days):(day+2*days-1)],Data$Settle[(day+days):(day+2*days-1)]))
      low2 <- min(c(Data$Low[(day+days):(day+2*days-1)],Data$Settle[(day+days):(day+2*days-1)]))
      tr2 <- high2-low2                   
      ATR <- rbind(ATR,mean(c(tr1,tr2),na.rm=TRUE))
  }
  for(day2 in 1:(period*days))
  {
    ATR <- rbind(ATR,0)
  }
  unname(ATR)
}

# Private/helper function for MAD. 
# For a moving average of specified length, returns the absolute deviation
#     for each day in the data.
getAD <- function(period=5,days=1)
{
  AD <- c()
  prange <- period*days-days
  dayRange <- seq(0,prange,days)
  for(day in 1:(nrow(Data)-prange))
  {
    mean <- mean(Data$Settle[(day+dayRange)],na.rm=TRUE)  
    AD <- rbind(AD,abs(Data$Settle[day]-mean)/mean)
  }
  for(day2 in 1:prange)
  {
    n <- prange-day2
    dayRange <- seq(0,n,days)
    mean <- mean(Data$Settle[(day+dayRange)],na.rm=TRUE)
    AD <- rbind(AD,abs(Data$Settle[(day+day2)]-mean)/mean)
  }
  AD <- round(AD,4)
  unname(AD)
}

#
# Gets Mean Absolute Deviation for a specified period
#     of specified days.
getMAD <- function(period=5,days=1)
{
  MAD <- c()
  prange <- period*days-days
  dayRange <- seq(0,prange,days)
  AD <- getAD(period,days)
  for(day in 1:(nrow(AD)-prange))
  {
    MAD <- rbind(MAD,mean(AD[(day+dayRange)]))
  }
  for(day2 in 1:prange)
  {
    n <- prange-day2
    dayRange <- seq(0,n,days)
    mean <- mean(MAD[(day+dayRange)],na.rm=TRUE)
    MAD <- rbind(MAD,mean)
  }
  MAD*100
}


# For every day in the data, returns the auto corelation with specified delay
#   for a specified period of specified days.
getAutoCor <- function(period=5,delay=1,days=1)
{
  ACor <- c()
  prange <- (period-1)*days
  dayRange <- seq(0,prange,days)
  for(day in 1:(nrow(Data)-prange-delay*days))
  { 
    x <- dayRange+day
    x2 <- dayRange+day+delay*days
    ACor <- rbind(ACor,cor(Data$Settle[x],Data$Settle[x2]))
  }
  for(miss in 1:(prange+delay*days))
  {
    ACor <- rbind(ACor,0)
  }
  round(unname(ACor),4)
}

# Helper method for calculating the DMA. 
#   Returns the xDay moving average for every day in the Data.
getMA <- function(period=10)
{
  MA <- c()
  for(day in 1:(nrow(Data)-period+1))
  {
    MA <- rbind(MA,mean(Data$Settle[day:(day+period-1)],na.rm=TRUE))
  }
  for(day2 in 1:(period-1))
  {
    MA <- rbind(MA,(sum(Data$Settle[(day+day2):(day+period-1)],na.rm=TRUE)/(period-day2)))
  }
  MA
}

# Returns the difference from the specified moving average
#   for every day in the data.
getDMAs <- function(period=10)
{
  MA <- getMA(period)
  DMA <- c()
  for(day in 1:nrow(MA))
  {
    DMA <- rbind(DMA,((Data$Settle[day]-MA[day])/MA[day]))
  }
  round(DMA*100,4)
}

# Given a statistic, for each day will return streak count for number of 
#   days above (or below) a certain value.
getStreak <- function(Statistic, absValue=0, positive=TRUE)
{ 
  if(positive!=TRUE)
    Statistic <- Statistic*-1
  days <-c()
  days <- data.frame(Statistic, count = NA)
  days$count[nrow(days)] <- ifelse(days$Statistic[nrow(days)] > absValue, 1, 0)
  for(i2 in 1:(nrow(days)-1))
  {
    i <- nrow(days)-i2
    days$count[i] <- ifelse(days$Statistic[i] > absValue, days$count[i + 1] + 1, 0)  
  }
  days$count
}


# For each day of the given Statistic, will identify rghtmost outliers with a 1 
#   and everything else with a dummy variable of 0.
getOutliers <- function(Statistic)
{
  meanS <- mean(Statistic,na.rm=TRUE)
  sdS <- sd(Statistic,na.rm=TRUE)
  twoSigma <- qnorm(.02,mean=meanS,sd= sdS, lower.tail=FALSE)
  i <- which(Statistic>twoSigma)
  dummy <- c(rep(0,length(Statistic)))
  dummy[i] <- 1
  dummy
}