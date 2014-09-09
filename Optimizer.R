
# Given a statistic to be used as the independent, its Name,
#   will test against a given dependent statistic. 
#   (Also need to specify name,days and period so there is no overlap)
# to return a data frame with 6 columns
# "Independent","x%","Dependent","y%","prob","X","N"

getOptimum <- function(statisticI,iName,dName,days,periods)
{
    # Arbitrary relationship requirement 
    probThreshold <- .65
    highprobs <- data.table()
    # Different lengths of the dependent var
    for(N2 in 1:length(days))
    {
        name <- paste(days[N2],dName,sep="")
        nameRef <- paste("X",days[N2],dName,sep="")
        statisticD <- Stats1[,nameRef]
        # Independent > x
        for(x in seq(1,10))
        {
            i <- which(statisticI >= x)
            i2 <- i-periods[N2]
            # Elimates signals that haven't happened yet
            notDays <- which(i2<0)
            if(length(notDays)>0)
              i2 <- i2[-notDays]
            # Dependent > y
            for(y in seq(1,10))
            {
                # Number of Hits
                hits <- length(which(statisticD[i2]>y))
                # Number of Triggers
                n <- length(i2)
                p <- hits/n
                p <- round(p,4)
                if(hits>0 && p>probThreshold && n>30)
                  highprobs <- rbind(highprobs,t(c(iName,">",x,name,">",y,p,hits,n)))
            }
            # Dependent < y
            for(y in seq(0,-10))
            {
                # Number of Hits
                hits <- length(which(statisticD[i2]<y))
                # Number of Triggers
                n <- length(i2)
                p <- hits/n
                p <- round(p,4)
                if(hits>0 && p>probThreshold && n>30)
                  highprobs <- rbind(highprobs,t(c(iName,">",x,name,"<",y,p,hits,n)))
            }
        }
        # Independent < x
        for(x in seq(0,-10))
        {
            i <- which(statisticI <= x)
            i2 <- i-periods[N2]
            # Elimates signals that haven't happened yet
            notDays <- which(i2<0)
            if(length(notDays)>0)
              i2 <- i2[-notDays]
            # Dependent > y
            for(y in seq(1,10))
            {
                # Number of Hits
                hits <- length(which(statisticD[i2]>y))
                # Number of Triggers
                n <- length(i2)
                p <- hits/n
                p <- round(p,4)
                if(hits>0 && p>probThreshold && n>30)
                  highprobs <- rbind(highprobs,t(c(iName,"<",x,name,">",y,p,hits,n)))
            }
            #Dependent < y
            for(y in seq(0,-10))
            {
                # Number of Hits
                hits <- length(which(statisticD[i2]<y))
                # Number of Triggers
                n <- length(i2)
                p <- hits/n
                p <- round(p,4)
                if(hits>0 && p>probThreshold && n>30)
                  highprobs <- rbind(highprobs,t(c(iName,"<",x,name,"<",y,p,hits,n)))
            }
        }  
    }
    highprobs
}
