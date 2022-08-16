require(srvyr)

svysummary <- function(svy_des, c_vars = character(),p_vars = character(),g_vars = character(),id_vars = character()){
  
  if(!identical(id_vars,character(0))){
    svy_des <- svy_des %>% 
      group_by_at(vars(one_of(id_vars))) 
    
  }
  
  summary_survey_continuous <-  summary_survey_proportions <-  summary_survey_groups <- data.frame()
  
  if(!identical(c_vars,character(0))){
    summary_survey_continuous <- svy_des  %>% 
      summarize_at(vars(one_of(c_vars)),
                   .funs = list(mean = ~survey_mean(.,na.rm=TRUE,vartype="ci"))) %>% 
      pivot_longer(cols=-one_of(id_vars),names_to=c("variable","est"),names_sep ="_mean") %>% 
      mutate(est = case_when(est == "_low" ~ "lci",
                             est == "_upp" ~ "uci",
                             TRUE ~ "estimate")) %>% 
      pivot_wider(names_from=est,values_from=value) %>% 
      mutate(type = "Continuous")
  }
  
  if(!identical(p_vars,character(0))){
    summary_survey_proportions <- svy_des %>% 
      summarize_at(vars(one_of(p_vars)),
                   .funs = list(mean = ~survey_mean(.,na.rm=TRUE,vartype="ci"))) %>% 
      pivot_longer(cols=-one_of(id_vars),names_to=c("variable","est"),names_sep ="_mean") %>% 
      mutate(est = case_when(est == "_low" ~ "lci",
                             est == "_upp" ~ "uci",
                             TRUE ~ "estimate")) %>% 
      mutate(value = value*100) %>% 
      pivot_wider(names_from=est,values_from=value) %>% 
      mutate(type = "Proportion")
    
    }
  
  if(!identical(g_vars,character(0))){
  summary_survey_groups <- map_dfr(g_vars,
                                   function(g){
                                     
                                     svy_des %>% 
                                       group_by_at(vars(id_vars,g)) %>% 
                                       summarize(proportion = survey_mean(na.rm = TRUE,vartype="ci")) %>% 
                                       rename(group = g) %>% 
                                       mutate(variable = g,
                                              group = as.character(group))
                                     
                                   }) %>% 
    dplyr::filter(!is.na(proportion)) %>% 
    rename(estimate = proportion,
           lci = proportion_low,
           uci = proportion_upp) %>% 
    mutate(type = "Grouped")
  }
  
  
  bind_rows(summary_survey_continuous,
            summary_survey_proportions,
            summary_survey_groups %>% 
              mutate_if(is.numeric,~.*100)) %>% 
    return(.)
  

  
}