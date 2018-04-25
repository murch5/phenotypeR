#Extract all regional classifications

regional_class <- as.data.frame(unique(pheno_data[,c("BirthCountry","Pt_Ethnicity","MGM_Ethnicity","MGF_Ethnicity","PGM_Ethnicity","PGF_Ethnicity")]))


regional_classifier_list <- apply(regional_class, 2, function(y){
  
  unique_id <- unique(y)
  return(unique_id)
  
})

regional_classifier_list <- unique(unlist(regional_classifier_list))

con<-file("regional_classifiers.csv", encoding="UTF-8")
write.csv(regional_classifier_list,file=con,row.names = FALSE)

headers <- as.data.frame(colnames(pheno_data))