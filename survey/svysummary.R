require(srvyr)

svysummary <- function(svy_des, c_vars = character(),
                       p_vars = character(),
                       g_vars = character(),
                       id_vars = character(),
                       remove_missing_levels = TRUE){
  
  if(!identical(id_vars,character(0))){
    svy_des <- svy_des %>% 
      group_by_at(vars(one_of(id_vars))) 
    
  }
  
  summary_survey_continuous <-  summary_survey_proportions <-  summary_survey_groups <- data.frame()
  
  if(!identical(c_vars,character(0))){
    summary_survey_continuous <- svy_des  %>% 
      summarize_at(vars(one_of(c_vars)),
                   .funs = list(mean = ~survey_mean(.,na.rm=TRUE,vartype="ci"))) 
    
    if(length(c_vars)>1){
      summary_survey_continuous <- summary_survey_continuous %>% 
        pivot_longer(cols=-one_of(id_vars),names_to=c("variable","est"),names_sep ="_mean") %>% 
        mutate(est = case_when(est == "_low" ~ "lci",
                               est == "_upp" ~ "uci",
                               TRUE ~ "estimate")) %>% 
        pivot_wider(names_from=est,values_from=value) %>% 
        mutate(type = "Continuous")
    }
    
    if(length(c_vars)==1){
      summary_survey_continuous <- summary_survey_continuous %>% 
        pivot_longer(cols=-one_of(id_vars),names_to=c("est"),values_to="value") %>% 
        mutate(est = case_when(est == "mean_low" ~ "lci",
                               est == "mean_upp" ~ "uci",
                               TRUE ~ "estimate"),
               variable = c_vars) %>% 
        pivot_wider(names_from=est,values_from=value) %>% 
        mutate(type = "Continuous")
    }
    
  }
  
  if(!identical(p_vars,character(0))){
    summary_survey_proportions <- svy_des %>% 
      summarize_at(vars(one_of(p_vars)),
                   .funs = list(mean = ~survey_mean(.,proportion = TRUE,na.rm=TRUE,vartype="ci"))) 
    
    if(length(p_vars)>1){
      summary_survey_proportions <- summary_survey_proportions %>% 
        pivot_longer(cols=-one_of(id_vars),names_to=c("variable","est"),names_sep ="_mean") %>% 
        mutate(est = case_when(est == "_low" ~ "lci",
                               est == "_upp" ~ "uci",
                               TRUE ~ "estimate")) %>% 
        mutate(value = value*100) %>% 
        pivot_wider(names_from=est,values_from=value) %>% 
        mutate(type = "Proportion")
    }
    if(length(p_vars)==1){
      summary_survey_proportions <- summary_survey_proportions %>% 
        pivot_longer(cols=-one_of(id_vars),names_to=c("est"),values_to="value") %>% 
        mutate(est = case_when(est == "mean_low" ~ "lci",
                               est == "mean_upp" ~ "uci",
                               TRUE ~ "estimate"),
               value = value*100,
               variable = p_vars) %>% 
        pivot_wider(names_from=est,values_from=value) %>% 
        mutate(type = "Proportion")
    }
    
    
  }
  
  if(!identical(g_vars,character(0))){
  summary_survey_groups <- map_dfr(g_vars,
                                   function(g){
                                     
                                     svy_des %>% 
                                       filter_at(vars(g), all_vars(!is.na(.))) %>% 
                                       srvyr::group_by(pick(c(id_vars,g))) %>% 
                                       # srvyr::summarize(proportion = survey_prop(na.rm = TRUE,proportion=TRUE,vartype="ci")) %>% 
                                       srvyr::summarize(proportion = survey_mean(proportion = TRUE,na.rm = TRUE,vartype="ci")) %>% 
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