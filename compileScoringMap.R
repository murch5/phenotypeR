#
# COMPILE SCORING MAP FUNC
#
#
# Function used to compile scoring map for phenotypic features from file
# 
#
# - passes character containing fileName containing encodings
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


compileScoringMap <- function(fileName)
{
  
  scoringMapRaw <- readLines(fileName)
  
  totalMaps <- (length(scoringMapRaw)/3)
  
  maps <- list()
  
  for(i in 1:totalMaps){
    
    newMapID <- scoringMapRaw[1+(3*(i-1))]
    newMapKeys <- scoringMapRaw[2+(3*(i-1))]
    newMapVals <- scoringMapRaw[3+(3*(i-1))]
  
    newMapKeys <- strsplit(newMapKeys,",")
    newMapVals <- strsplit(newMapVals,",")
    
    newMap <- data.frame(newMapKeys,newMapVals,stringsAsFactors = FALSE)
    colnames(newMap) <- c("key","value")
    newMapSet <- list(newMapID,newMap)
    names(newMapSet) <- c("columnID","map")
    
    maps[[i]] <- newMapSet
  }
  
  return(maps)
}

#r <- compileScoringMap("test.scoreConfig")
