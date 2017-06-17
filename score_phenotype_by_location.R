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
#   recode_vals - data.frame containing phenotype ID with associated values (values according to severity of phenotypic classification)
#   weights_location - data.frame containing location ID and associated weightings (according to regions associated with greater disease severity)
#
# return:
#   output - data.frame containing new score values

library(dplyr)
library(plyr)

scorePhenotypeByLocation <-
  function(input, recode_vals, weights_location)
  {
    

    score <- apply(input, 1, function(x) {
      
      
      
      score_recoded <- as.numeric(recode_vals[which(recode_vals[, 1] == x["Involvement"]), 2])
      
      score_weighted <-
        score_recoded * as.numeric((weights_location[which(weights_location[, 1] == x["Site"]), 2]))
      
      if (length(score_recoded) < 1) {
        score_recoded = NA
      }
      if (length(score_weighted) < 1) {
        score_weighted = NA
      }
      
      score <- data.frame(site=x["Site"],recodeScore = as.numeric(score_recoded), Weighted_Score = as.numeric(score_weighted), stringsAsFactors = FALSE)
      
      return(score)
      
    })
    
    score <- bind_rows(score)
    score <- unique(score)
    score <- score[,-1]
    
    colnames(score) <- c("Unweighted", "Weighted")
    
    score_summary <- score %>%
      summarise_all(.funs = c(sum = "sum",mean = "mean",sd = "sd"))

   
    return(score_summary)
  }


