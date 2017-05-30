#
# GROUP APPLY - ID WRAPPER FUNC
#
#
# Function used to apply groupings to data set according to IDs loaded from .csv files
# 
#
# - passes data.frame containing data set and path to groupings folder containing individual .csv files containing ID lists 
# - return data.frame with all grouping applied 
#
# arguments:
#   dataSet - data.frame containing data to be grouped
#   groupingsFolder - path to folder containing individual .csv for all groupings
#
# return:
#   output - data.frame containing data set with new column appended

source("groupByID.R")

groupApplyID <- function(dataSet,groupingsFolder)
{
  path <- paste("./",groupingsFolder,"/",sep="",collapse="")

  fileNames <- list.files(path)

  for(x in fileNames)
    {
    
    grouping <- read.csv2(paste(path,x,sep="",collapse = ""),stringsAsFactors = FALSE)
    
    grouping <- grouping[1]
  

    dataSet <- groupByID(dataSet,colnames(grouping),1,grouping)
  
  }
  
  
  return(dataSet)
}

#d <- groupApplyID(XIAPdata,"groups")