extract_chemicals <- function(webpage){
  
  chemicals = webpage$ChemicalList %>% 
    bind_rows() %>% 
    mutate_all(function(x) case_when(x == "NULL" ~ "",
                                     TRUE ~ as.character(x))) %>% 
    mutate(index = 1:n())
  
  
  return(chemicals)
  
  
}