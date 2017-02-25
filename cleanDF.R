#Clean data


cleanDataFrame <- function(data)
{
  
  temp <- data
  
  colnames(temp) <- gsub(x = colnames(temp),
                       pattern = "\\+",
                       replacement = "plus")
  
  colnames(temp) <- gsub(x = colnames(temp),
                       pattern = "[\u2014:\u2013:-]",
                       replacement = "-")
  
  colnames(temp) <- gsub(x = colnames(temp),
                       pattern = "\\.",
                       replacement = "_")
  
  colnames(temp) <- gsub(x = colnames(temp),
                         pattern = "\\/",
                         replacement = "_")
  
  
  temp <- apply(temp,c(1,2),function(y){
    
    
    return(gsub(x = y,
         pattern = "[\u2014:\u2013:-]",
         replacement = "-"))
    
    
  })
  
  temp <- apply(temp,c(1,2),function(y){
    
    
    return(trimws(y))
    
    
  })
  
  temp <- as.data.frame(temp,stringsAsFactors=FALSE)
  
  temp <- apply(temp,c(1,2),function(y){
    
    
    return(gsub(x = y,
                pattern = "[,<><\UFF1C\UFF1E]",
                replacement = ""))
    
    
  })
  
  temp <- as.data.frame(temp,stringsAsFactors=FALSE)
  
  return(temp)
  
  
}
