table1_summary <- function(df, c_vars = character(),
                           p_vars = character(),
                           g_vars = character(),
                           id_vars = character(),
                           remove_missing_levels = TRUE){
  
  
  
  if(!identical(id_vars,character(0))){
    df <- df %>% 
      group_by_at(vars(one_of(id_vars))) 
    
  }
 
  table1_summary_continuous <-  table1_summary_proportions <-  table1_summary_groups <- data.frame()
  
  if(!identical(c_vars,character(0))){
    table1_summary_continuous <- df  %>% 
      summarize(across(one_of(c_vars),
                   .fns = list(mean = ~mean(.,na.rm=TRUE),
                                sd = ~ sd(.,na.rm=TRUE),
                                median = ~median(.,na.rm=TRUE),
                                q25 = ~quantile(.,0.25,na.rm=TRUE),
                                q75 = ~quantile(.,0.75,na.rm=TRUE),
                                min = ~min(.,na.rm = TRUE),
                                max = ~max(.,na.rm=TRUE),
                                freq = ~sum(!is.na(.)),
                               missing = ~sum(is.na(.))),
                   .names = "{col}__{fn}")) 
    
    if(length(c_vars)>=1){
      table1_summary_continuous <- table1_summary_continuous %>% 
        pivot_longer(cols=-one_of(id_vars),names_to=c("variable","est"),names_sep ="__") %>% 
        # pivot_wider(names_from=est,values_from=value) %>% 
        mutate(type = "Continuous")
    }
    
    # if(length(c_vars)==1){
    #   summary_survey_continuous <- summary_survey_continuous %>% 
    #     pivot_longer(cols=-one_of(id_vars),names_to=c("est"),values_to="value") %>% 
    #     mutate(est = case_when(est == "mean_low" ~ "lci",
    #                            est == "mean_upp" ~ "uci",
    #                            TRUE ~ "estimate"),
    #            variable = c_vars) %>% 
    #     pivot_wider(names_from=est,values_from=value) %>% 
    #     mutate(type = "Continuous")
    # }
    
  }
  
  if(!identical(p_vars,character(0))){
    table1_summary_proportions <- df %>% 
      summarize(across(one_of(p_vars),
                   .fns = list(proportion = ~mean(.,na.rm=TRUE),
                                freq = ~sum(!is.na(.)),
                               missing = ~sum(is.na(.))),
                   .names = "{col}__{fn}")) 
    
    if(length(p_vars)>=1){
      table1_summary_proportions <- table1_summary_proportions %>% 
        pivot_longer(cols=-one_of(id_vars),names_to=c("variable","est"),names_sep ="__") %>% 
        mutate(value = case_when(est == "proportion" ~ value*100,
                                 TRUE ~ value)) %>% 
        # pivot_wider(names_from=est,values_from=value) %>% 
        mutate(type = "Proportion")
    }
    # if(length(p_vars)==1){
    #   summary_survey_proportions <- summary_survey_proportions %>% 
    #     pivot_longer(cols=-one_of(id_vars),names_to=c("est"),values_to="value") %>% 
    #     mutate(est = case_when(est == "mean_low" ~ "lci",
    #                            est == "mean_upp" ~ "uci",
    #                            TRUE ~ "estimate"),
    #            value = value*100,
    #            variable = p_vars) %>% 
    #     pivot_wider(names_from=est,values_from=value) %>% 
    #     mutate(type = "Proportion")
    # }
    
    
  }
  
  if(!identical(g_vars,character(0))){
    table1_summary_groups <- map_dfr(g_vars,
                                     function(g){
                                       
                                       df %>% 
                                         filter_at(vars(g), all_vars(!is.na(.))) %>% 
                                         group_by(pick(c(id_vars))) %>% 
                                         mutate(n = n()) %>% 
                                         ungroup() %>% 
                                         group_by(pick(one_of(c(id_vars,g)))) %>% 
                                         dplyr::summarize(proportion = 100*n()/n,
                                                          freq = n()) %>%
                                         ungroup() %>% 
                                         distinct(pick(one_of(c(id_vars,g))),.keep_all = TRUE) %>% 
                                         pivot_longer(cols=-one_of(c(id_vars,g)),names_to="est",values_to="value") %>% 
                                         rename(group = g) %>% 
                                         mutate(variable = g,
                                                group = as.character(group))
                                       
                                     }) %>% 
      dplyr::filter(!is.na(value)) %>% 
      mutate(type = "Grouped")
  }
  
  
  bind_rows(  table1_summary_continuous,
              table1_summary_proportions,
              table1_summary_groups ) %>% 
    return(.)
  
  
  
  
  
  
  
}