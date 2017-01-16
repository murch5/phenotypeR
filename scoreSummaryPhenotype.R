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

library(dplyr)
library(plyr)

source("scorePhenotype.R")

scoreSummaryPhenotype <- function(input,recodeValues, locationWeights)
{
  
  d <-
    input %>% 
    group_by(MuiseLabID, Tbl_Encounter.Timing, Date, Ix) %>%
    select(.,Site,Involvement)  %>% 
    do(scorePhenotypeByLocation(., recodeValues, locationWeights))
  
  
  return(d)
}

ddt <- scoreSummaryPhenotype(testData,data.frame(c("Not Involved", "Macroscopic Disease"), c(0, 3)), data.frame(c("Left Colon", "Perianal"), c(1, 3)))