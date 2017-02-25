#
# GROUP BY ID 
#
#
# Function used to create a grouping variable column based on a passed list of IDs
# 
#
# - passes data.frame containing data set, the ID column name, the name of the new group, and the list of IDs to include in new grouping 
# - return data.frame with new columns appended to end
#
# arguments:
#   dataSet - data.frame containing data to be grouped
#   IDcol - column name of ID column
#   groupName - name of the new group (will become new column name)
#   IDlist - list of ID values to include in new grouping
#
# return:
#   output - data.frame containing data set with new column appended

groupByID <- function(dataSet,groupName,IDcol,IDlist)
{
  
  IDlist <- IDlist[[1]]
  print(IDlist)
  
  newColumn <- apply(dataSet,1,function(y)
  {
    
    catVal <- 0
    
    ID <- as.numeric(y[IDcol])
    
    if((ID %in% IDlist))
  {
    catVal = 1
  }else
  {

    catVal = 0
  }
    
    return(catVal)
    
  })
  
  newColumn <- as.data.frame(newColumn,stringsAsFactors=FALSE)
  colnames(newColumn) <- groupName
  
  dataSet <- cbind(dataSet,newColumn)
  
  return(dataSet)
}

