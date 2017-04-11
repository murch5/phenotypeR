#TTC7A Pheno Processing II

library(dplyr)
library(stringr)
source("cleanDF.R")


parseTimeDays <- function(time)
{
  
  
  pattern <- "([0-9.]*[a-z])"
  timeSep <- str_extract_all(time, pattern)
 
  timeSep <- timeSep[[1]]
  
  timeDays <- lapply(timeSep,function(y)
    {
    
    timeSum = 0
    
    patternNum <- "([0-9.]*)"
    patternType <- "([A-Za-z])"
    num <- str_extract_all(y, patternNum)
    type <- str_extract_all(y, patternType)   
    
    type <- type[[1]]
    num <- as.numeric(num[[1]])
    
    
    if(type=="d")
    {
      
      timeSum = timeSum + num
    }
    else if(type=="y")
    {
     
      timeSum = timeSum + (num * 365)
    }
    else if(type=="m")
    {
      
      timeSum = timeSum + (num * 30)
    }
  
    return(timeSum)
    
  })

  timeDays <- unlist(timeDays)

 # temp <- Reduce("+",timeDays)
  temp <- sum(timeDays, na.rm=TRUE)

  return(temp)
  
}

parseTimeDaysToYears <- function(time)
{
  return(time*365)
  
}

parseTimeCol <- function(dataSet, indices)
{
  temp <- apply(dataSet[indices], c(1, 2), function(x) {
    t <- parseTimeDays(x)
    return(t)
  })
  
  colnames(temp) <- paste(colnames(temp), "Days", sep = "_")
  
  
  
  dataSet <- cbind(dataSet, temp)
  
  return(dataSet)
  
}

