getStats1 <- function()
{
  Statistics <- data.table(Data$Date)
  setnames(Statistics,"V1","Date")

  Statistics$Close <- Data$Settle

  xDayPMRange <- c(1:20,50,100,200)
  for(i in 1:length(xDayPMRange))
  {
    name <- paste(xDayPMRange[i],"DayPercentMove",sep="")
    Statistics[,name] <- getPercentMove(period=xDayPMRange[i])
  }
  
  xDayAutoCorRange <- c(10,20,50,100)
  for(i in 1:length(xDayAutoCorRange))
  {
    name <- paste(xDayAutoCorRange[i],"DayAutoCor",sep="")
    Statistics[,name] <- getAutoCor(days=xDayAutoCorRange[i])
  }
  
  xDayATRRange <- c(1,2,5,10,20,50,100)
  for(i in 1:length(xDayATRRange))
  {
    name <- paste(xDayATRRange[i],"DayATR",sep="")
    Statistics[,name] <- getATR(days=xDayATRRange[i])
  }
  
  xDayMADRange <- c(1,2,3,5,10,20,50,100,200)
  for(i in 1:length(xDayMADRange))
  {
    name <- paste(xDayMADRange[i],"DayMAD",sep="")
    Statistics[,name] <- getMAD(days=xDayMADRange[i])
  }
  
  xDayMARange <- c(20,50,100)
  for(i in 1:length(xDayMARange))
  {
    name <- paste(xDayMARange[i],"DayMA",sep="")
    Statistics[,name] <- getMA(period=xDayMARange[i])
  }
  
  xDayDMARange <- c(20,50,100)
  for(i in 1:length(xDayDMARange))
  {
    name <- paste(xDayDMARange[i],"DayDMA",sep="")
    Statistics[,name] <- getDMAs(period=xDayDMARange[i])
  }
  
  write.csv(Statistics, "Stats1.csv",row.names=FALSE)
}