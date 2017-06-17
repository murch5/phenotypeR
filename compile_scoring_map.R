#
# COMPILE SCORING MAP FUNC
#
#
# Function used to compile scoring map for phenotypic features from file
# 
#
# - passes character containing file_name containing encodings
#
# -creates various scoring columns according to arguments passed
# - returns data.frame with new score columns
#
# arguments:
#   input - data.frame containing subset of phenotypic data to be analyzed
#   recodeValues - data.frame containing phenotype ID with associated values (values according to severity of phenotypic classification)
#   locationWeights - data.frame containing location ID and associated weightings (according to regions associated with greater disease severity)
#
# return:
#   output - data.frame containing new score values


compileScoringMap <- function(file_name)
{
  
  map_scoring_raw <- readLines(file_name)
  
  map_scoring_list <- (length(map_scoring_raw)/3)
  
  maps <- list()
  
  for(i in 1:map_scoring_list){
    
    map_ID <- map_scoring_raw[1+(3*(i-1))]
    map_keys <- map_scoring_raw[2+(3*(i-1))]
    map_vals <- map_scoring_raw[3+(3*(i-1))]
  
    map_keys <- strsplit(map_keys,",")
    map_vals <- strsplit(map_vals,",")
    
    map_new <- data.frame(map_keys,map_vals,stringsAsFactors = FALSE)
    colnames(map_new) <- c("key","value")
   
  
    maps[[i]] <- map_new
  }

 maps <- as.data.frame(unlist(maps, recursive=FALSE))
  return(maps)
}

