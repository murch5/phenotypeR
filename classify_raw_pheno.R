#Classify raw phenotype data

compileRawMap <- function(file_name)
{
  
  map_scoring_raw <- readLines(file_name)
  
  map_list <- (length(map_scoring_raw)/3)
  
  maps <- list()
  
  for(i in 1:map_list){
    
    map_ID <- map_scoring_raw[1+(3*(i-1))]
    map_type <- map_scoring_raw[2+(3*(i-1))]
    map_val <- map_scoring_raw[3+(3*(i-1))]
    
    
    map_val <- strsplit(map_val,",")
    
    map_set <- list(map_ID,map_type,map_val)
    names(map_set) <- c("columnID","type","map")
    
    maps[[i]] <- map_set
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
    
    temp_val <- input[input %in% unlist(map[3][[1]])]
    temp_val <- paste(temp_val,collapse=";")
    temp <- temp_val
  }
  
  return(temp)
  
  
}

iterateSet <- function(data_set,maps)
{
  
  data <- lapply(maps,function(x){
    
    data_mapped <- lapply(data_set,function(y){
      
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
    
    data_mapped_unlist <- as.data.frame(unlist(data_mapped),stringsAsFactors=FALSE)
    
    colnames(data_mapped_unlist) <- x[[1]][[1]]
    return(data_mapped_unlist)
  })
  
  return(as.data.frame(data,stringsAsFactors=FALSE))
  
}

rawClassify <- function(data_set,parse_target,mappings_list)
{
  
  data_classified <- iterateSet(parse_target,mappings_list)
  
  data_set <- cbind(data_set,data_classified)
  
  return(data_set)
}
