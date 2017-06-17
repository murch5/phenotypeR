#
# COLLAPSE FUNCTION FUNCTION
#
#
# Function used to collapse individual phenotype data from observation format (tidy form)
#
# - passes data.frame containing data_set, indices of the columns to be categorized, and category hash table
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
  col_names <- colnames(entry)
  
  data_flat <- apply(entry,2,function(x)
    {
    
    entry_unique <- unique(x)
    
    entry_unique <- entry_unique[!is.na(entry_unique)]
    
    entry_unique <- entry_unique[which(entry_unique!="")]
    
    if(length(entry_unique)<1){entry_unique=NA}
    
    if(length(entry_unique)>1)
    {
      entry_unique <- paste(entry_unique,collapse=";")
      
    }
    
    return(as.data.frame(entry_unique, stringsAsFactors = FALSE))
  })
  
  data_flat <- as.data.frame(data_flat,stringsAsFactors = FALSE)
  colnames(data_flat) <- col_names
  data_flat[,1] <- as.numeric(data_flat[,1])
  
  return(data_flat)
  
}
collapse <- function(data_set,index_col)
{
  
  data_flattened <<- data_set %>%
    group_by(MuiseLabID) %>%
    do(flattenEntry(.))
  
  
return(data_flattened)
  
}

#test5 <- phenoMaster[,c("MuiseLabID","EIM","InitDx Categorized")]
#test6 <- collapse(test5,1)