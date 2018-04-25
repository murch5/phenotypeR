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

add_full_path <- function(path, filename){
  return(paste(path,filename, sep=""))
}