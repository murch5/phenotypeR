
suppressPackageStartupMessages(library(dplyr))


merge_df_by_key <- function(base_df, base_key, hash_df, hash_key, prefix = NULL)
{
  if(is.null(prefix))
  {
 new_df <- base_df %>%
    dplyr::left_join(hash_df,by = setNames(hash_key, base_key))
  }
  else
  {
    hash_df_prefix <- hash_df
    
    colnames(hash_df_prefix) <- lapply(colnames(hash_df_prefix), function(y){
      
      if(y != hash_key)
      {
        return(paste(prefix,"_",y,sep=""))
      }
      else
      {
        return(y)
      }
      
        
    })
    
    new_df <- base_df %>%
      dplyr::left_join(hash_df_prefix,by = setNames(hash_key, base_key))
    
  }
  
  return(new_df)
}