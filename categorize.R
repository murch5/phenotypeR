#
# CATEGORIZE FUNCTION
#
#
# Helper function used to categorize input based on category hash table 
#
# - passes input value and category hash table and returns category value
# - if key value has no associated hash, return "NoVal"
#
# arguments:
#   input - value to be categorized
#   categoryHash - hash table containing categories to be mapped
#
# return:
#   output - hash table converted value (typically as character)

categorize <- function(input, categoryHash)
{
  
  
  if (is.na(input)) {
    categorizedData <- NA
    
  } else{
    categorizedData <-
      categoryHash[which(categoryHash[, 1] == input), 2]
  }
  
  if (length(categorizedData) < 1) {
    categorizedData <- NA
  }
  
  return(categorizedData)
  
}