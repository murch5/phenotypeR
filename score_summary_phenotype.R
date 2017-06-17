#
# SCORE SUMMARY - PHENOTYPE FUNCTION
#
#
# Function used to score and compile location site-specific phenotypes compiled by ID, Encounter Type, Date, and Ix
# Adds covariates to aggregrate score values
#
# - passes data.frame containing phenotypic , data.frame containing values to recoded phenotypic categories
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

source("score_phenotype.R")
source("compile_scoring_map.R")

library(plyr)
library(dplyr)

scoreSummaryPhenotype <- function(input,score_map)
{
  
  recode_val <- score_map[[1]]
  weights <-  score_map[[2]]
  map_features <- score_map[c(3:(length(score_map)))]

  score_map <-
    input %>% 
    group_by(MuiseLabID, Tbl_Encounter_Timing, Date, Ix) %>%
    select(.,Site,Involvement, Luminal, EIM)  %>% 
    do(scorePhenotype(., recode_val,weights,map_features))
  
  
  score_aggregate_by_ID <<-
    score_map %>%
    ungroup %>%
    group_by(MuiseLabID) %>%
    dplyr::summarize(.,aggregateMean.ByID = mean(modelUnweighted), aggregateSD.ByID = sd(modelUnweighted))
  
  score_aggregate_by_Enc <<-
    score_map %>%
    ungroup %>%
    group_by(MuiseLabID,Tbl_Encounter_Timing) %>%
    dplyr::summarize(.,aggregateMean.ByEncounter = mean(modelUnweighted), aggregateSD.ByEncounter = sd(modelUnweighted))
  
  score_aggregate_by_Date <<-
    score_map %>%
    ungroup %>%
    group_by(MuiseLabID,Tbl_Encounter_Timing,Date) %>%
    dplyr::summarize(.,aggregateMean.ByDate = mean(modelUnweighted), aggregateSD.ByDate = sd(modelUnweighted))

    
  phenotypeScore <- list(score_map,score_aggregate_by_Date,score_aggregate_by_Enc,score_aggregate_by_Date)
  
  return(score_map)
}
