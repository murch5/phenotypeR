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

library(plyr) #include plyr library
library(dplyr) #include dplyr library

source("categorize.R")  #include source for categorize function

categorizeMutate <- function(dataSet, indices, categoryHash)
{
 
  mutate
  
}