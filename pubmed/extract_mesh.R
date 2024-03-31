extract_mesh <- function(webpage){
  mesh = webpage$MeshHeadingList %>% 
    bind_rows() %>% 
    mutate_all(function(x) case_when(x == "NULL" ~ "",
                                     TRUE ~ as.character(x))) 
  
  
  mesh_out = tryCatch({
    mesh %>% 
      mutate(index = 1:n())  %>% 
      pivot_longer(cols=contains("QualifierName"),names_to="name",values_to="val") %>% 
      dplyr::filter(!is.na(val)) %>% 
      group_by(DescriptorName,index) %>% 
      summarize(QualifierName = paste0(val,collapse=", ")) %>% 
      ungroup() %>% 
      mutate(QualifierName = str_replace(QualifierName,"(^,\\s,\\s|,\\s$)",""))},
    error = function(e){
      data.frame(DescriptorName = NA_character_,
                 QualifierName = NA_character_,
                 index = NA_real_,
                 PMID = NA_real_)
    })
  
  
  return(mesh_out)
  
  
}