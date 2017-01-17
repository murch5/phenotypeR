#
# SCORE PHENOTYPE BY FEATURE FUNCTION
#
#
# Function used to score individual phenotypic features according to passed weightings of associated severity etc.
#
# - passes data.frame containing subset of phenotypic information to be score, data.frame containing values to recoded phenotypic categories
#   and data.frame containing weighting characteristic of each location
# -creates various scoring columns according to arguments passed
# - returns data.frame with new score columns
#
# arguments:
#   input - data.frame containing subset of phenotypic data to be analyzed
#   recodeValues - data.frame containing phenotype ID with associated values (values according to severity of phenotypic classification)
#   featureMapping - data.frame containing feature  and associated weightings (according to regions associated with greater disease severity)
#
# return:
#   output - data.frame containing new score values

scorePhenotypeByFeature <- function(input,featureMapping)
{
  
  scoresFeature <- t(lapply(featureMapping, function(x){
    
    temp <- unique(input[,x[["columnID"]]])
    
    unweightedScore <- x[["map"]][which(x[["map"]][,"type"]==temp),"value"]
    if(length(unweightedScore)<1){unweightedScore = 0}
    return(unweightedScore)
  }))
  
  scoresFeature <- as.data.frame(scoresFeature)
  
  return(scoresFeature)
}

testData <- read.csv("test.csv",stringsAsFactors=FALSE)
testData2 <- testData[c(2:8), c("Luminal", "EIM")]

luminalMap <- data.frame(type = c("","Inflammatory"), value = c(0,1),stringsAsFactors = FALSE)
EIM <- data.frame(type = c("","IDDM"), value = c(0,1),stringsAsFactors = FALSE)

l <- list("Luminal",luminalMap)
names(l) <- c("columnID","map")
e <- list("EIM",EIM)
names(e) <- c("columnID","map")

featureMapping <- list(l,e)

gg <- scorePhenotypeByFeature(testData2,featureMapping)