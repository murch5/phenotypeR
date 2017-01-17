#
# SCORE PHENOTYPE WRAPPER FUNCTION
#
#
# Function used to call phenotype scoring by both location and feature from within dplyr do()
#
# - passes data.frame containing phenotypic information, data.frame containing values to recoded phenotypic categories
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


source("scorePhenotypeLocation.R")
source("scorePhenotypeFeature.R")

scorePhenotype <- function(dataSet,recodeValues, locationWeights, featureMapping)
{
  
  site <- scorePhenotypeByLocation(dataSet, recodeValues, locationWeights)
  feature <- scorePhenotypeByFeature(dataSet,featureMapping)

  scoreCombined <- cbind(site,feature)
  
  return(scoreCombined)
}