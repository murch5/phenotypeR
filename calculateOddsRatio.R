#
# CALCULATE ODDS RATIOS FUNCTION
#
#
# Function used to determine odds ratios using the R package epitools
#
# - passes data.frame containing data set to be process, the case-control column used for "disease", and a list of covariate columns used as the "exposure"
# - calculates and extracting odds ratio, confidence intervals and p-values for each level of exposure per covariate
# - returns data.frame with odds ratio scored with an individual row for each exposure level
#
# arguments:
#   dataSet - data.frame containing data to be process
#   caseControlCond - string containing the name of the column to be used as the "disease" classification (must be binary)
#   covarList - list of strings representing columns to be scored; each exposure level will be calculated according to factors of specific column
#
# return:
#   oddsSet - returns data.frame containing odds ratios calculations sorted by exposure and the individual covariate

library(dplyr)
library(epitools)

compileOddsRatioData <-
  function(dataSet, caseControlCond, covarList)
  {
    oddsRatioList <- lapply(covarList, function(y) {
      contig <-
        calculateContigencyTable(dataSet[, c(caseControlCond, y)], caseControlCond, y)
      oddsSet <- extractOddsData(contig)
      
      return(oddsSet)
      
    })
    
    oddsRatioDF <- bind_rows(oddsRatioList)
    
    return(oddsRatioDF)
  }

extractOddsData <- function(oddsData)
{
  
  temp <<-oddsData
  datadf <- oddsData[[1]]
  ORdf <- oddsData[[2]]
  pvaldf <- oddsData[[3]]
  
  disease <- names(dimnames(datadf))[2]
  name <- names(dimnames(ORdf))[1]
  
  #Add loop to iterate over all levels
  
  
  levels <- dimnames(datadf)[[1]]
  
  
  compiledOddsData <- data.frame()
  
  for (i in 1:(length(levels) - 1))
  {
 
    OR <- ORdf[i, 1]
    LCI <- ORdf[i, 2]
    UCI <- ORdf[i, 3]
    pMid <- pvaldf[i,1]
    pFish <- pvaldf[i,2]
    pChi <- pvaldf[i,3]
    level <- levels[i]

    newOddsData <-
      data.frame(
        Disease = disease,
        Exposure = name,
        Level = level,
        Odds_Ratio = OR,
        LCI = LCI,
        UCI = UCI,
        pMid = pMid,
        pFish = pFish,
        pChi = pChi,
        stringsAsFactors = FALSE
      )
    
    colnames(newOddsData) <-
      c("Disease", "Exposure", "Level", "OR", "LCI", "UCI","pMid","pFish","pChi")
    
    compiledOddsData <- rbind(compiledOddsData, newOddsData)
    
  }

  return(compiledOddsData)
  
}

calculateContigencyTable <- function(data, caseControlCond, covar)
{
  summarizedDataInclNA <- data %>%
    group_by_(.dots = c(caseControlCond, covar)) %>%
    tally()
  
  summarizedData <- data %>%
    group_by_(.dots = c(caseControlCond, covar))
  
  contigTable <- table(summarizedData)
  
  contigTrans <- t(contigTable)
  
  
  oddsData <- calculateOddsRatio(contigTrans)
  
  return(oddsData)
}

calculateOddsRatio <- function(contigTable)
{
  oddsRatio <- oddsratio.fisher(contigTable, NULL)
  
  return(oddsRatio)
  
}

#prunedData <- data %>%
#  filter_(.dots = paste0("!is.na(",caseControlCond, ")"))

#print(prunedData)
