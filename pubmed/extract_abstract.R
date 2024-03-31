
extract_abstract <- function(webpage){
  abstract = webpage$Article$Abstract %>% 
    unlist() %>% 
    paste0(.,collapse="") %>% 
    as.character()
  
  return(abstract)
  
  
}