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
#   recode_vals - data.frame containing phenotype ID with associated values (values according to severity of phenotypic classification)
#   map_feature - data.frame containing feature  and associated weightings (according to regions associated with greater disease severity)
#
# return:
#   output - data.frame containing new score values


source("score_phenotype_by_location.R")
source("score_phenotype_by_feature.R")
source("score_model_phenotype.R")

scorePhenotype <- function(data_set,recode_vals, weights_location, map_feature)
{
  
  site <- scorePhenotypeByLocation(data_set, recode_vals, weights_location)
  
  feature <- scorePhenotypeByFeature(data_set,map_feature)

  score_combined <- cbind(site,feature)
  
  model <- scoreModelPhenotype(score_combined,0)
  
  score_set <- cbind(score_combined,model)
  
  return(score_set)
}