#Classify raw phenotype data

compileRawMap <- function(fileName)
{
  
  scoringMapRaw <- readLines(fileName)
  
  totalMaps <- (length(scoringMapRaw)/3)
  
  maps <- list()
  
  for(i in 1:totalMaps){
    
    newMapID <- scoringMapRaw[1+(3*(i-1))]
    newMapType <- scoringMapRaw[2+(3*(i-1))]
    newMapVals <- scoringMapRaw[3+(3*(i-1))]
    
    
    newMapVals <- strsplit(newMapVals,",")
    
    newMapSet <- list(newMapID,newMapType,newMapVals)
    names(newMapSet) <- c("columnID","type","map")
    
    maps[[i]] <- newMapSet
  }
  
  return(maps)
}


checkMapBinary <- function(input,map)
{
  
  temp <- 0
  
  if(length(input)<1)
  {
    temp <- NA
  }
  
  if (any(input %in% unlist(map[3][[1]])))
  {
    
    temp <- 1
  }
  
  return(temp)
  
}

checkMapGetVal <- function(input,map)
{
  
  temp <- "NR"
  
  if(length(input)<1)
  {
    temp <- NA
  }
  
  if (any(input %in% unlist(map[3][[1]])))
  {
    
    tempString <- input[input %in% unlist(map[3][[1]])]
    tempString <- paste(tempString,collapse=";")
    temp <- tempString
  }
  
  return(temp)
  
  
}

iterateSet <- function(dataset,maps)
{
  
  data <- lapply(maps,function(x){
    
    mappedData <- lapply(dataset,function(y){
      
      if(x[[2]][[1]]=="Any")
      {
        r <- checkMapBinary(y,x)
      }
      else if(x[[2]][[1]]=="Val")
      {
        r <- checkMapGetVal(y,x)
      }
      return(r)
    })
    
    temp <- as.data.frame(unlist(mappedData),stringsAsFactors=FALSE)
    
    colnames(temp) <- x[[1]][[1]]
    return(temp)
  })
  
  return(as.data.frame(data,stringsAsFactors=FALSE))
  
}

rawClassify <- function(dataset,parseTarget,mappingsList)
{
  
  classifiedData <- iterateSet(parseTarget,mappingsList)
  
  dataset <- cbind(dataset,classifiedData)
  
  return(dataset)
}
