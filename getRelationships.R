getRelationships <- function()
{

#

#  Goes through running the optimize algorithim to find relationships P >.8
#   for any percent move.


  pmDays <- c(1:20)
  pmDaysI <- c(1:20,50,100,200)
  relationshipsPM <- data.table("Independent","","x%","Dependent","","y%","prob","X","N")

  # For each Independent Variable (xDay Percent Moves Columns) we test against every XDay Percent move
  for(pm in pmDaysI)
  {
    iName <- paste(pm," Day Percent Move",sep="")  
    statisticI <- Stats1[,paste("X",pm,"DayPercentMove",sep="")]
    relationshipsPM<- rbind(relationshipsPM,getOptimum(statisticI,iName,"DayPercentMove",pmDays,pmDays))  
  }

  madDays2 <- c(1,2,3,5,10,20,50,100,200)
  # For each Independent Variable (MAD Columns) we test against every XDay Percent move
  for(mad in madDays2)
  {
    iName <- paste(mad," Day MAD",sep="")
    statisticI <- Stats1[,paste("X",mad,"DayMAD",sep="")]
    relationshipsPM<- rbind(relationshipsPM,getOptimum(statisticI,iName,"DayPercentMove",pmDays,pmDays))  
  }

  dmaDays <- c(20,50,100)
  # For each Independent Variable (DMA Columns) we test against every XDay Percent move
  for(dma in dmaDays)
  {
    iName <- paste(dma," Day DMA",sep="")
    statisticI <- Stats1[,paste("X",dma,"DayDMA",sep="")]
    relationshipsPM<- rbind(relationshipsPM,getOptimum(statisticI,iName,"DayPercentMove",pmDays,pmDays))  
  }
  write.csv(relationshipsPM, "relationshipsPM.csv",row.names=FALSE)
}