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
#   data_set - data.frame containing data to be process
#   case_control_cond - string containing the name of the column to be used as the "disease" classification (must be binary)
#   covar_list - list of strings representing columns to be scored; each exposure level will be calculated according to factors of specific column
#
# return:
#   oddsSet - returns data.frame containing odds ratios calculations sorted by exposure and the individual covariate

suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(epitools))

compileOddsRatioData <-
  function(data_set, case_control_cond, covar_list)
  {
    odds_ratio_list <- lapply(covar_list, function(y) {
      contig <-
        calculateContigencyTable(data_set[, c(case_control_cond, y)], case_control_cond, y)
      oddsSet <- extractOddsData(contig)
      
      return(oddsSet)
      
    })
    
    odds_ratio_df <- bind_rows(odds_ratio_list)
    
    return(odds_ratio_df)
  }

extractOddsData <- function(odds_data)
{
  
  data_df <- odds_data[[1]]
  odds_ratio_df <- odds_data[[2]]
  pval_df <- odds_data[[3]]
  
  disease <- names(dimnames(data_df))[2]
  name <- names(dimnames(odds_ratio_df))[1]
  
  #Add loop to iterate over all levels
  
  
  levels <- dimnames(data_df)[[1]]
  
  
  odds_data_compiled <- data.frame()
  
  for (i in 1:(length(levels) - 1))
  {
 
    OR <- odds_ratio_df[i, 1]
    LCI <- odds_ratio_df[i, 2]
    UCI <- odds_ratio_df[i, 3]
    pMid <- pval_df[i,1]
    pFish <- pval_df[i,2]
    pChi <- pval_df[i,3]
    level <- levels[i]

    odds_data_new <-
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
    
    colnames(odds_data_new) <-
      c("Disease", "Exposure", "Level", "OR", "LCI", "UCI","pMid","pFish","pChi")
    
    odds_data_compiled <- rbind(odds_data_compiled, odds_data_new)
    
  }

  return(odds_data_compiled)
  
}

calculateContigencyTable <- function(data, case_control_cond, covar)
{
  summarizedDataInclNA <- data %>%
    group_by_(.dots = c(case_control_cond, covar)) %>%
    tally()
  
  summarizedData <- data %>%
    group_by_(.dots = c(case_control_cond, covar))
  
  config_table <- table(summarizedData)
  
  contig_transposed <- t(config_table)
  
  
  odds_data <- calculateOddsRatio(contig_transposed)
  
  return(odds_data)
}

calculateOddsRatio <- function(config_table)
{
  odds_ratio <- oddsratio.fisher(config_table, NULL)
  
  return(odds_ratio)
  
}

