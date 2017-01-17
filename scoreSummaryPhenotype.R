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

scoreSummaryPhenotype <- function(input,recodeValues, locationWeights, featureMapping)
{
  
  d <-
    input %>% 
    group_by(MuiseLabID, Tbl_Encounter.Timing, Date, Ix) %>%
    select(.,Site,Involvement, Luminal, EIM)  %>% 
    do(scorePhenotype(., recodeValues, locationWeights,featureMapping)) 
    

  return(d)
}

#Mappings for test functions
recode <- data.frame(c("Not Involved", "Macroscopic Disease"), c(0, 3))
locationWeight <- data.frame(c("Left Colon", "Perianal"), c(1, 3))

luminalMap <- data.frame(type = c("","Inflammatory"), value = c(0,1),stringsAsFactors = FALSE)
EIM <- data.frame(type = c("","IDDM"), value = c(0,1),stringsAsFactors = FALSE)

l <- list("Luminal",luminalMap)
names(l) <- c("columnID","map")
e <- list("EIM",EIM)
names(e) <- c("columnID","map")

featureMapping <- list(l,e)

ddt <- scoreSummaryPhenotype(testData,recode, locationWeight, featureMapping)