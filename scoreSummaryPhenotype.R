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

source("scorePhenotype.R")
source("compileScoringMap.R")

library(plyr)
library(dplyr)

scoreSummaryPhenotype <- function(input,scoreMapping)
{
  
  print(scoreMapping[3:length(scoreMapping)])
  print(length(scoreMapping))
  
  scoreSummary <-
    input %>% 
    group_by(MuiseLabID, Tbl_Encounter.Timing, Date, Ix) %>%
    select(.,Site,Involvement, Luminal, EIM)  %>% 
    do(scorePhenotype(., scoreMapping[[1]][[2]], scoreMapping[[2]][[2]],scoreMapping[c(3:(length(scoreMapping)))]))
  
  print(scoreSummary)
  
  aggregateByID <<-
    scoreSummary %>%
    ungroup %>%
    group_by(MuiseLabID) %>%
    dplyr::summarize(.,aggregateMean.ByID = mean(modelUnweighted), aggregateSD.ByID = sd(modelUnweighted))
  
  aggregateByEnc <<-
    scoreSummary %>%
    ungroup %>%
    group_by(MuiseLabID,Tbl_Encounter.Timing) %>%
    dplyr::summarize(.,aggregateMean.ByEncounter = mean(modelUnweighted), aggregateSD.ByEncounter = sd(modelUnweighted))
  
  aggregateByDate <<-
    scoreSummary %>%
    ungroup %>%
    group_by(MuiseLabID,Tbl_Encounter.Timing,Date) %>%
    dplyr::summarize(.,aggregateMean.ByDate = mean(modelUnweighted), aggregateSD.ByDate = sd(modelUnweighted))

    
  phenotypeScore <- list(scoreSummary,aggregateByDate,aggregateByEnc,aggregateByDate)
    
  
    ddd<<-phenotypeScore

  return(scoreSummary)
}

#Mappings for test functions

testData <- testData[c(1:10000),] #subset to speed testing

recode <- data.frame(c("Not Involved", "Macroscopic Disease"), c(0, 3))
locationWeight <- data.frame(c("Left Colon", "Perianal"), c(1, 3))

luminalMap <- data.frame(type = c("","Inflammatory"), value = c(0,1),stringsAsFactors = FALSE)
EIM <- data.frame(type = c("","IDDM"), value = c(0,1),stringsAsFactors = FALSE)

l <- list("Luminal",luminalMap)
names(l) <- c("columnID","map")
e <- list("EIM",EIM)
names(e) <- c("columnID","map")

featureMapping <- list(l,e)


r <- compileScoringMap("test.scoreConfig")
c <- (r[[1]][[2]])
q <- (r[[2]][[2]])


#ddt <- scoreSummaryPhenotype(testData,r)