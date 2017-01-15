#
# CATEGORIZE FUNCTION
#
#
# Helper function used to categorize input based on category hash table 
#
# - passes input value and category hash table and returns category value
#
# arguments:
#   input - value to be categorized
#   categoryHash - hash table containing categories to be mapped
#
# return:
#   output - hash table converted value (typically as character)

library(plyr) #include plyr library

categorize <- function(input,categoryHash)
{
  
    if(is.na(input)) {
      categorizedData <- "NA"
      
    }else{ 
      
      categorizedData <- categoryHash[which(categoryHash[,1]==input),2]
      print(categorizedData)
    }


 return(categorizedData)
  
}