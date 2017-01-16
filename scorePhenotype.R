#
# SCORE PHENOTYPE BY LOCATION FUNCTION
#
#
# Function used to score individual phenotypes according to passed weightings at a set of disease locations
#
# - passes data.frame containing subset of phenotypic information to be score, data.frame containing values to recoded phenotypic categories
#   and data.frame containing weighting characteristic of each location
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

library(dplyr)

scorePhenotypeByLocation <- function(input,recodeValues,locationWeights)
{
  
  scores <- t(as.data.frame(apply(input,1,function(x){
          
             recodedScore <- recodeValues[which(recodeValues[,1]==x[2]),2]
             weightedScore <- recodedScore*(locationWeights[which(locationWeights[,1]==x[1]),2])
             
             if(length(recodedScore)<1){recodedScore = NA}
             if(length(weightedScore)<1){weightedScore = NA}
             
             score <- c(recodedScore,weightedScore)
             print(score)
             
    return(score)
    
  })))
  
  colnames(scores) <- c("Unweighted","Weighted")
  
  totals <- colSums(scores, na.rm = TRUE)
  
  locationScores <- data.frame(totals[1],totals[2])
  colnames(locationScores) <- c("Total Unweighted","Total Weighted")
  return(locationScores)
}

testData <- read.csv("test.csv")
testData2 <- testData[c(2:8),c(16,17)]
t <- scorePhenotypeByLocation(testData2,data.frame(c("Not Involved","Macroscopic Disease"),c(0,3)),data.frame(c("Left Colon","Perianal"),c(1,3)))

