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
#   data_set - data.frame containing columns to be categorized
#   indices - vector of columns indices to be categorized
#   hash_category - hash table containing values to be remapped
#
# return:
#   output - data.frame containg data_set with new category columns appended adjacent to original columns

source("categorize.R")  #include source for categorize function

categorizeMutate <- function(data_set, indices, hash_category)
{
  data_mutated <- apply(data_set[indices], c(1, 2), function(x) {
    t <- categorize(x, hash_category)
    return(t)
  })
  
  colnames(data_mutated) <- paste(colnames(data_mutated), "Categorized", sep = " ")
  
  data_set <- cbind(data_set, data_mutated)
  
  return(data_set)
  
}

