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
#   locationWeights - data.frame containing location ID and associated weightings (according to regions associated with greater disease severity)
#
# return:
#   output - data.frame containing new score values

scorePhenotypeByFeature <- function(input,)