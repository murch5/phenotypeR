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
#   hash_category - hash table containing categories to be mapped
#
# return:
#   output - hash table converted value (typically as character)

categorize <- function(input, hash_category)
{
  
  
  if (is.na(input)) {
    data_categorized <- NA
    
  } else{
    data_categorized <-
      hash_category[which(hash_category[, 1] == input), 2]
  }
  
  if (length(data_categorized) < 1) {
    data_categorized <- NA
  }
  
  return(data_categorized)
  
}