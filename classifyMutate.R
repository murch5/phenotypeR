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


evaluateSetCriteria <- function(classifiedData, classifyHash)
{
  q <- apply(classifiedData,1, function(y) {
    result <- all(y, na.rm = FALSE)
    
    if (result == TRUE)
    {
      value = classifyHash[[2]]
    }
    else
    {
      value = classifyHash[[3]]
    }
    return(value)
    
    
  })
  
  q <- as.data.frame(q, stringsAsFactors = FALSE)
  colnames(q) <- classifyHash[1]
  setCriteriaAdded <- cbind(classifiedData, q)
  return(setCriteriaAdded)
}
evaluateCriteria <- function(dataSubset, criteria, classifyHash)
{
  dataEvaluated <- data.frame()
 colNames <- character()
 
  r <- lapply(criteria, function(x) {
    d <- apply(dataSubset, 1, function(y) {
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
    
    classifiedData <- as.data.frame(d, stringsAsFactors = FALSE)
    
    name <- paste(x, collapse = "")
    colnames(classifiedData) <- as.character(name)
    colNames <<- c(colNames,as.character(name))
    return(classifiedData)
    
  })
  r <- as.data.frame(r, stringsAsFactors = FALSE)
  colnames(r) <- colNames
  
  
  fullSet <- evaluateSetCriteria(r, classifyHash)
  
  dataSubset <- cbind(dataSubset, fullSet)
  return(dataSubset)
  
}

classifyMutate <-
  function(dataSet,
           filters,
           columns,
           criteria,
           classifyHash)
  {
    d <-
      dataSet %>%
      filter_(., filters) %>%
      select_(., columns) %>%
      do(evaluateCriteria(., criteria, classifyHash))
    
    return(d)
    
  }

test <- read.csv("test2.csv", stringsAsFactors = FALSE)
test <- test[c(1:100),]
categoryHashtest <-
  data.frame(c(1, 2, 16984), c("Test1", "Test2", "Test2"), stringsAsFactors = FALSE)


crit <- data.frame("paternalID", ">", "1", stringsAsFactors = FALSE)
crit2 <- data.frame("paternalID", "<", "1", stringsAsFactors = FALSE)
critList <- list(crit, crit2)
classify <- list("ALL", "A1", "A2")

test2 <-
  classifyMutate(test[, -1],
                 as.character("sexID>1"),
                 "paternalID",
                 critList,
                 classify)
