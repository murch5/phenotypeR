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
  
 categorizedData <- apply(input[,-1],2,function(y){
  
    if(is.na(y)) {
      mappedData <- "Temp"
      
    }else{
      
      mappedData <- as.data.frame(categoryHash[which(categoryHash[,1]==y),2],stringsAsFactors = FALSE)
    }
 
   return(as.data.frame(mappedData,stringsAsFactors = FALSE))
    
  })

 return(as.data.frame(categorizedData,stringsAsFactors = FALSE))
  
}