#
# CATEGORIZE AND MUTATE FUNCTION
#
#
# Function to categorize a subset of columns in a data.frame while returning a new data.frame with new columns appended (adjacent to categorized columns)
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

source("categorize.R")  #include source for categorize function
library(dplyr)


evaluateSetCriteria <- function(data_classified, hash_classify)
{
  data_set_criteria <- apply(data_classified, 1, function(y) {
    result <- all(y, na.rm = FALSE)
    
    if (result == TRUE)
    {
      value = hash_classify[[2]]
    }
    else
    {
      value = hash_classify[[3]]
    }
    return(value)
    
    
  })
  
  
  data_set_criteria <- as.data.frame(data_set_criteria, stringsAsFactors = FALSE)
  colnames(data_set_criteria) <- hash_classify[1]
  data_compiled <- cbind(data_classified, data_set_criteria)
  return(data_compiled)
}

evaluateCriteria <- function(data_subset, criteria, hash_classify)
{
  dataEvaluated <- data.frame()
  col_names <- character()
  
  r <- lapply(criteria, function(x) {
    d <- apply(data_subset, 1, function(y) {
      evalExp <- c("y[\"", (x[, 1]), "\"]", x[, 2], x[, 3])
      evalExp <- paste(evalExp, collapse = "")
      
      if (eval(parse(text = evalExp)))
      {
        flag = TRUE
      } else
      {
        flag = FALSE
      }
      
      return(flag)
    })
    
    data_classified <- as.data.frame(d, stringsAsFactors = FALSE)
    
    name <- paste(x, collapse = "")
    colnames(data_classified) <- as.character(name)
    col_names <<- c(col_names, as.character(name))
    return(data_classified)
    
  })
  r <- as.data.frame(r, stringsAsFactors = FALSE)
  colnames(r) <- col_names
  
  
  data_fullset <- evaluateSetCriteria(r, hash_classify)
  
  data_subset <- cbind(data_subset, data_fullset)
  return(data_subset)
  
}

classifyMutate <-
  function(dataSet,
           filters,
           columns,
           criteria,
           hash_classify)
  {
    d <-
      dataSet %>%
      filter_(., filters) %>%
      select_(., columns) %>%
      do(evaluateCriteria(., criteria, hash_classify))
    
    return(d)
    
  }


