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
#   data_set - data.frame containing data to be grouped
#   id_col - column name of ID column
#   group_name - name of the new group (will become new column name)
#   id_list - list of ID values to include in new grouping
#
# return:
#   output - data.frame containing data set with new column appended

groupByID <- function(data_set,group_name,id_col,id_list)
{
  
  id_list <- id_list[[1]]
  
  
  column_new <- apply(data_set,1,function(y)
  {
    
    cat_val <- 0
    
    ID <- as.numeric(y[id_col])
    
    if((ID %in% id_list))
  {
    cat_val = 1
  }else
  {

    cat_val = 0
  }
    
    return(cat_val)
    
  })
  
  column_new <- as.data.frame(column_new,stringsAsFactors=FALSE)
  colnames(column_new) <- group_name
  
  data_set <- cbind(data_set,column_new)
  
  return(data_set)
}

