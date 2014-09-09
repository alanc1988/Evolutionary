# Downloads the Data from www.quandl.com, Stores it in the workspace and fixes the missing Data.
getData <- function(mode=1)
{
  
  date <- unlist(strsplit(date(), split=" "))
  day <- as.integer(date[3])
  year <- as.integer(date[5])
  month <- date[2]
  if(month=="Aug"){
    month <- 8
  }else if(month=="Sept"){
    month <- 9
  }

  if(mode==1)
  {
    url <- paste('http://www.quandl.com/api/v1/datasets/CHRIS/CME_ES1.csv?&trim_start=1997-09-09&trim_end=',year,'-',month,'-',day,'&sort_order=desc',sep="")
    Data <- data.table(read.csv(url, colClasses=c('Date'='Date')))
    
    ### Fixs the few dates where data is missing/flawed  
    i <- which(Data$Date=="2001-09-14")
    Data$Low[i] <- 1020
    Data$Low[i+1] <- 1020
    Data$High[i] <- 1103
    Data$High[i+1] <- 1103
    i <- which(Data$Date=="2001-04-13")
    Data$Low[i] <- 1173
    
  }
  if(mode==2)
  {
    url <- paste("http://real-chart.finance.yahoo.com/table.csv?s=%5EVIX&d=",month,"&e=",day,"&f=",year,"&g=d&a=0&b=2&c=1990&ignore=.csv",sep="")
    Data <- read.csv(url,colClasses=c('Date'='Date')) 
  }
  
  Data
  
}
