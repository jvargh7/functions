extract_affiliations <- function(webpage){
    
    affiliation = webpage$Article$AuthorList %>% 
      imap_dfr(.,
               function(x,i){
                 
                 aff_list = x %>% 
                   map_depth(1,"Affiliation") %>% 
                   unlist() %>%
                   data.frame() 
                 
                 if(nrow(aff_list) == 0){
                   data.frame(index = 1, AffiliationInfo = NA) %>% 
                     return(.)
                   
                 }else{
                   aff_list %>% 
                     pivot_longer(cols=everything(),names_to="index",values_to="AffiliationInfo") %>% 
                     mutate(index = 1:n()) %>% 
                     return(.)
                 }
                 
                 
               })   %>% 
      mutate(author_id = case_when(index == 1 ~ cumsum(index),
                                   TRUE ~ NA_real_)) %>% 
      mutate(author_id = zoo::na.locf(author_id)) %>% 
      dplyr::select(author_id,index,AffiliationInfo)
    
    return(affiliation)
    
    
    
  }
  
  

