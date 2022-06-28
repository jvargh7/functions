
svymean_wide2long <- function(wide_table,prefix = "_dm$",id_cols = character(),type = "ci"){
  print(prefix)
  
  if(type == "ci"){
    prefix_set = paste0("(",prefix,"|",
                        str_replace(prefix,"\\$","_low$"),"|",
                        str_replace(prefix,"\\$","_upp$"),")")
    
    long_table <- wide_table %>% 
      dplyr::select(one_of(id_cols),
                    matches(prefix_set)) %>% 
      pivot_longer(cols=matches(prefix_set),
                   names_to = "variable",
                   values_to = "prevalence") %>% 
      mutate(estimate = case_when(str_detect(variable,"low$") ~ "lci",
                                  str_detect(variable,"upp$") ~ "uci",
                                  TRUE ~ "est"),
             
             variable = str_replace(prefix,"_","") %>% str_replace(.,"\\$","")
      ) %>% 
      pivot_wider(names_from=estimate,values_from=prevalence) %>% 
      mutate(std_err = (est - lci)/1.96)
  }
  
  if(type == "se"){
    prefix_set = paste0("(",prefix,"|",
                        str_replace(prefix,"\\$","_se$)"))
    
    long_table <- wide_table %>% 
      dplyr::select(one_of(id_cols),
                    matches(prefix_set)) %>% 
      pivot_longer(cols=matches(prefix_set),
                   names_to = "variable",
                   values_to = "prevalence") %>% 
      mutate(estimate = case_when(str_detect(variable,"se$") ~ "std_err",
                                  TRUE ~ "est"),
             
             variable = str_replace(prefix,"_","") %>% str_replace(.,"\\$","")
      ) %>% 
      pivot_wider(names_from=estimate,values_from=prevalence) %>% 
      mutate(lci = (est - 1.96*std_err),
             uci = (est + 1.96*std_err))
  }
  
  return(long_table)
  
  
}