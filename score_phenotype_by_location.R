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

suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(plyr))

determine_involvement <-
  function(input,
           subset,
           operation,
           condition = NULL,
           val = NULL) {
    set <- unlist(subset)
    subset_data <-
      input[which(input[, "Site"] %in% set, arr.ind = TRUE),]
    
    return_val <- 0
    
    if (all(is.na(subset_data[, "Unweighted"])))
    {
      return_val <- 0
      
    }
    else
    {
      if (operation == "all")
      {
        subset_fail <-
          subset_data[which(subset_data[, "Unweighted"] == 0), "Unweighted"]
        if (length(subset_fail > 0))
        {
          return_val <- 2
        }
      }
      else if (operation == "any")
      {
        if (sum(subset_data[, "Unweighted"], na.rm = TRUE) > 0)
        {
          return_val <- 2
        }
        else
        {
          return_val <- 1
        }
      }
      else if (operation == "sum")
      {
        if (condition == ">")
        {
         
          if (length(which(subset_data[, "Unweighted"] > 0)) > as.integer(val))
          {
            return_val <- 2
          }
          else
          {
            return_val <- 1
          }
        }
        
      }
      
    }
    
    return(return_val)
  }


loc_groups <-
  read.table(
    "/home/murch/DATA/project/pheno_geno_correlation/raw/config/location_groups_inv",
    sep = ";",
    header = TRUE
  )

make_location_groupings <- function(data)
{
  output <- apply(loc_groups, 1, function(y) {
    subset_region <- strsplit(y["group"], split = ",")
    val <-
      determine_involvement(data, subset_region, y["operation"], y["condition"], y["val"])
    
    return(val)
  })
  
  location_group_df <-
    data.frame(inv = loc_groups[, "loc"],
               inv_code = output,
               stringsAsFactors = FALSE)
  
  return(location_group_df)
}


scorePhenotypeByLocation <-
  function(input, recode_vals, weights_location)
  {
    score <- apply(input, 1, function(x) {
      score_recoded <-
        as.numeric(recode_vals[which(recode_vals[, 1] == x["Involvement"]), 2])
      
      score_weighted <-
        score_recoded * as.numeric((weights_location[which(weights_location[, 1] == x["Site"]), 2]))
      
      if (length(score_recoded) < 1) {
        score_recoded = NA
      }
      if (length(score_weighted) < 1) {
        score_weighted = NA
      }
      
      score <-
        data.frame(
          site = x["Site"],
          recodeScore = as.numeric(score_recoded),
          Weighted_Score = as.numeric(score_weighted),
          stringsAsFactors = FALSE
        )
      
      return(score)
      
    })
    
    score <- bind_rows(score)
    score <- unique(score)
    colnames(score) <- c("Site", "Unweighted", "Weighted")
    
    
    score_category <- score
    
    
    location_group_scores <- make_location_groupings(score_category)
    
    location_group_scores <- t(location_group_scores)
    colnames(location_group_scores) <- location_group_scores[1, ]
    
    location_group_df <- as.data.frame(location_group_scores)
    location_group_df <- location_group_df[c(2), ]
    
    score <- score[, -1]
    
    
    
    #### ADD CATEGORIZING FOR DIFFERENT DISEASE LOCATIONS ETC
    
    
    score_summary <<- score %>%
      summarise_all(.funs = c(sum = "sum", mean = "mean", sd = "sd"))
    
    
    
    score_summary <<- cbind(score_summary, location_group_df)
    
    return(score_summary)
  }
