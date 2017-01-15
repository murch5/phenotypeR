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

categorizeMutate <- function(dataSet, indices, categoryHash)
{
 
  temp <- apply(dataSet[indices],c(1,2), function(x){
    t <- categorize(x,categoryHash)
    return(t)
  })
  
  colnames(temp) <- paste(colnames(temp),"Categorized", sep=" ")
  
  dataSet <- cbind(dataSet[,-1],temp)
  
  return(dataSet)
  
}

test <- read.csv("test2.csv", stringsAsFactors = FALSE)
categoryHashtest <- data.frame(c(0,1,2,16984),c("Z","Test1","Test2","Test2"),stringsAsFactors = FALSE)
test2 <- categorizeMutate(test,c(6,7),categoryHashtest)
