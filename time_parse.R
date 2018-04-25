#TTC7A Pheno Processing II

suppressPackageStartupMessage(library(dplyr))
suppressPackageStartupMessages(library(stringr))
source("clean_DF.R")


parseTimeDays <- function(time)
{
  
  
  pattern <- "([0-9.]*[a-z])"
  time_sep <- str_extract_all(time, pattern)
 
  time_sep <- time_sep[[1]]
  
  time_days <- lapply(time_sep,function(y)
    {
    
    time_sum = 0
    
    pattern_num <- "([0-9.]*)"
    pattern_type <- "([A-Za-z])"
    num <- str_extract_all(y, pattern_num)
    type <- str_extract_all(y, pattern_type)   
    
    type <- type[[1]]
    num <- as.numeric(num[[1]])
    
    
    if(type=="d")
    {
      
      time_sum = time_sum + num
    }
    else if(type=="y")
    {
     
      time_sum = time_sum + (num * 365)
    }
    else if(type=="m")
    {
      
      time_sum = time_sum + (num * 30)
    }
  
    return(time_sum)
    
  })

  time_days <- unlist(time_days)

 # temp <- Reduce("+",time_days)
  temp <- sum(time_days, na.rm=TRUE)

  return(temp)
  
}

parseTimeDaysToYears <- function(time)
{
  return(time*365)
  
}

parseTimeCol <- function(data_set, indices)
{
  temp <- apply(data_set[indices], c(1, 2), function(x) {
    t <- parseTimeDays(x)
    return(t)
  })
  
  colnames(temp) <- paste(colnames(temp), "Days", sep = "_")
  
  
  
  data_set <- cbind(data_set, temp)
  
  return(data_set)
  
}

