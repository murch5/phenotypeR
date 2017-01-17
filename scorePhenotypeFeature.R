

scorePhenotypeByFeature <- function(input,featureMapping)
{
  
  scoresFeature <- lapply(featureMapping, function(x){
    
    temp <- unique(input[,x[["columnID"]]])
    unweightedScore <- x[["map"]][which(x[["map"]][,"type"]==as.character(temp)),"value"]
    if(length(unweightedScore)<1){unweightedScore = 0}
    
    unweightedScore <- as.data.frame(as.numeric(unweightedScore))
    colnames(unweightedScore) <- paste(x[["columnID"]],"SumUnweighted",sep=" ")
    return(unweightedScore)
  })
  
  scoresFeature <- as.data.frame(scoresFeature)
  return(scoresFeature)
}

testData <- read.csv("test.csv",stringsAsFactors=FALSE,check.names=FALSE)
testData2 <- testData[c(2:8), c("Luminal", "EIM")]

luminalMap <- data.frame(type = c("","Inflammatory"), value = c(0,1),stringsAsFactors = FALSE)
EIM <- data.frame(type = c("","IDDM"), value = c(0,1),stringsAsFactors = FALSE)

l <- list("Luminal",luminalMap)
names(l) <- c("columnID","map")
e <- list("EIM",EIM)
names(e) <- c("columnID","map")

featureMapping <- list(l,e)

gg <- scorePhenotypeByFeature(testData2,featureMapping)