#
# COLLAPSE FUNCTION FUNCTION
#
#
# Function used to collapse individual phenotype data from observation format (tidy form)
#
# - passes data.frame containing dataset, indices of the columns to be categorized, and category hash table
# - creates one new column for each column categorized
# - returns data.frame with new columns insert after categorized columns
#
# arguments:
#   dataSet - data.frame containing columns to be categorized
#   indices - vector of columns indices to be categorized
#   categoryHash - hash table containing values to be remapped
#
# return:
#   output - data.frame containg dataSet with new category columns appended adjacent to original columns

library(dplyr)

flattenEntry <- function(entry)
{
  colName <- colnames(entry)
  
  flatData <- apply(entry,2,function(x)
    {
    
    uniqEntry <- unique(x)
    
    uniqEntry <- uniqEntry[!is.na(uniqEntry)]
    
    uniqEntry <- uniqEntry[which(uniqEntry!="")]
    
    if(length(uniqEntry)<1){uniqEntry=NA}
    
    if(length(uniqEntry)>1)
    {
      uniqEntry <- paste(uniqEntry,collapse=";")
      
    }
    
    return(as.data.frame(uniqEntry, stringsAsFactors = FALSE))
  })
  f <- as.data.frame(flatData,stringsAsFactors = FALSE)
  colnames(f) <- colName
  f[,1] <- as.numeric(f[,1])
  
  return(f)
  
}
collapse <- function(dataset,indexColumn)
{
  
  flattenedSet <<- dataset %>%
    group_by(MuiseLabID) %>%
    do(flattenEntry(.))
  
  
return(flattenedSet)
  
}

#test5 <- phenoMaster[,c("MuiseLabID","EIM","InitDx Categorized")]
#test6 <- collapse(test5,1)