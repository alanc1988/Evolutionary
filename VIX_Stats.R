#Times to Revert from High to Normal
revertFrom <- function(high,normal,lag=20)
{
  days <- which(VIX$Close>high)
  
  # Only looks at crosses which have been below the specified high for 20 (lag) days previously.
  lastCross <- days[length(days)]
  temp <- c(lastCross)
  for(day in (length(days)-1):1)
  {
    i <- days[day]
    if(i<=(lastCross-lag))
      temp <- c(i,temp)
    lastCross <- i
  } 
  days <- temp
  
  # Calculates the time it takes from these days to regress to the value specified
  timeToRevert <- rep(0,length(days))
  for(day in length(days):1)
  {
    i <- days[day]
    # with many variables a high value here will last a LONG TIME.
    for(i2 in 1:2000)
    {
      if((i-i2)<1)
      {          
        # If it is still curently above the specified value from the last cross.
        timeToRevert[day] <- 0
        break
      }
      if(VIX$Close[i-i2]<=normal)
      {
        timeToRevert[day] <- i2
        break
      }
    }
  }
  #timeToRevert
  mean <- mean(timeToRevert)
  median <- median(timeToRevert)
  sd <- sd(timeToRevert)
  n <- length(timeToRevert)
  min <- min(timeToRevert)
  max <- max(timeToRevert)
  output <- rbind(mean,median,sd,n,min,max)
  round(output,3)
}

getReversionTimes <- function(high=20,normal=c(-5:-1),lag=20)
{
  labels <- c("mean","median","sd","n","min","max")
  output <- data.frame(labels)
  for(i in 1:length(normal))
  {
    name <- paste("DaysFrom",high,"To",(high+normal[i]),sep="")
    output[,name] <- revertFrom(high,(high+normal[i]))
  }
  output
}

#Time to Hit x, given that it has been 20% below 20 for the last 20 days.
timeToHit <- function(high,lag=20,lagPercent = .2)
{
  days <- which(VIX$Close>((1-lagPercent)*high))
  
  # Only looks at crosses which have been a percent below the high for 20 (lag) days previously.
  lastCross <- days[length(days)]
  temp <- c(lastCross)
  for(day in (length(days)-1):1)
  {
    i <- days[day]
    if(i<=(lastCross-lag))
      temp <- c(i,temp)
    lastCross <- i
  } 
  days <- temp
  
  # Calculates the time it takes from these days to regress to the value specified
  timeToHit <- c()
  for(day in length(days):1)
  {
    i <- days[day]
    # with many variables a high value here will last a LONG TIME.
    for(i2 in 1:2000)
    {
      if((i-i2)<1)
      {          
        # If it is still curently above the specified value from the last cross.
        break
      }
      if(VIX$Close[i-i2]>=high)
      {
        timeToHit <-c(i2,timeToHit)
        break
      }
    }
  }
  #timeToHit
  mean <- mean(timeToHit)
  median <- median(timeToHit)
  sd <- sd(timeToHit)
  n <- length(timeToHit)
  min <- min(timeToHit)
  max <- max(timeToHit)
  output <- rbind(mean,median,sd,n,min,max)
  round(output,3)
}

getHitTimes <- function(days=c(20,25,30))
{
  labels <- c("mean","median","sd","n","min","max")
  output <- data.frame(labels)
  for( i in 1:length(days))
  {
    name <- paste("TimeToHit",days[i],sep="")
    output[,name]=timeToHit(days[i])
  }
  output
  
}


getVIX_Cors <- function()
{
  Stats1 <- read.csv("Stats1.csv")
  Stats1$Date <- as.Date(Stats1$Date)
  
  earliest <- min(as.Date(Stats1$Date))
  cut <- which(VIX$Date==earliest)
  
  VIX_Stats <- data.frame(VIX[(1:cut),"Date"],row.names=NULL)
  names(VIX_Stats)[1] <- "Date"
  VIX_Stats[,"Close"] <- VIX[(1:cut),"Close"]
  
  notinc <- c() # Vector of which days we have SP & No VIX
  for(day in 1:length(Stats1$Date))
  {
    date <- Stats1$Date[day]
    if(length(which(VIX_Stats$Date==date))==0)
      notinc <- c(notinc,day)
  }
  Stats1 <- data.frame(Stats1[-notinc,],row.names=NULL)
  
  
  VIX_CORS <- data.frame(1)
  xDayRange <- c(1:20)
  for(i in 1:length(xDayRange))
  {
    namePM <- paste("X",xDayRange[i],"DayPercentMove",sep="")
    name <- paste("COR_VIX_",xDayRange[i],"DayPM",sep="")
    VIX_CORS[,name] <- cor(VIX_Stats[,"Close"],Stats1[,namePM])
  }
  VIX_CORS <- VIX_CORS[,-1]
  VIX_CORS 
  
}

getPercentMoveVIX <- function(period=1)
{
  PM <- c()
  for(day in 1:(nrow(VIX)-period))
  {
    move <- (VIX[day,"Close"]-VIX[day+period,"Close"])/VIX[day+period,"Close"]
    move <- signif(move,5)*100
    PM <- rbind(PM,move)
  }
  for(day in 1:period)
  {
    PM <- rbind(PM,0.0)
  }
  signif(unname(PM),4)
}

timeToPercentDrop <- function(timeFrameUp,runUp,drop,timeFrameDown)
{
  x1DayPMMoveVix <- getPercentMoveVIX(1)
  
  tVix <- getPercentMoveVIX(timeFrameUp)
  
  i <- which(tVix>runUp)
  lastCross = i[length(i)]
  
  dayI <- length(i)-1
  bucketsI <- c(lastCross)
  
  for(dayI in (length(i)-1):1)
  {
    if(i[dayI]<(lastCross-timeFrameUp))
    {
      lastCross <- i[dayI]
      bucketsI <- c(lastCross,bucketsI)
    }
  }
  
  hits <- c()
  
  for(i in length(bucketsI):1)
  {
    day <- bucketsI[i]
    for(i2 in 1:timeFrameDown)
    {
      temp <- day+i2
      if(x1DayPMMoveVix[temp] <= -1*drop)
      {    
        hits<- c(hits,i2)
        break
      }
    }
  }
  mean <- mean(hits)
  median <- median(hits)
  sd <- sd(hits)
  x <- length(hits)
  n <- length(bucketsI)
  min <- min(hits)
  max <- max(hits)
  output <- rbind(mean,median,sd,x,n,min,max)
  round(output,3)
}


