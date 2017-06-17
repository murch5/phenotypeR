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
#   map_feature - data.frame containing feature  and associated weightings (according to regions associated with greater disease severity)
#
# return:
#   output - data.frame containing new score values

scorePhenotypeByFeature <- function(input,map_feature)
{
  
  testing <<- map_feature
  
  map_feature_list <<- map_feature[length(map_feature)]
  i <- 0
  
  scoresFeature <- lapply(map_feature[1:length(map_feature)-1], function(x){
    i =+ 1
    temp <- unique(input[,map_feature_list[[i]]])
    
    score_unweighted <- x[which(x[,"key"]==as.character(temp)),"value"]
    if(length(score_unweighted)<1){score_unweighted = 0}
    
    score_unweighted <- as.data.frame(as.numeric(score_unweighted))
    colnames(score_unweighted) <- paste(map_feature_list[[i]],"Unweighted_sum",sep=" ")
    return(score_unweighted)
  })
  
  scoresFeature <- as.data.frame(scoresFeature)
  return(scoresFeature)
}
