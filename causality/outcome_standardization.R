outcome_standardization <- function(df=data.frame(),o_formula,
                                    treatment_var = "bin6775",ind_id = "id_uni",
                                    ipw=NA,levels_vec=NA){
  
  outcome_var = str_split(o_formula," ~ ")[[1]][1]
  
  updated_formula = paste0("y ~ a + ",paste0(str_split(o_formula,"\\+")[[1]][-1],collapse="+"))
  updated_formula = str_replace_all(updated_formula,treatment_var,"a")
  
  treatment_vec = df %>% 
    dplyr::select(treatment_var) %>% 
    pull()
  
  
  
  
  df <- df %>% 
    mutate_("a" = treatment_var,
            "y" = outcome_var,
            "id"=ind_id)
  
  # if(is.character(levels_vec)|is.factor(levels_vec)){
  #   df <- df %>%  
  #     mutate(a = case_when(a == levels_vec[1] ~ 0,
  #                          a == levels_vec[2] ~ 1,
  #                          TRUE ~ NA_real_)) 
  #   
  # }
  
  
  if(length(levels_vec)<2){
    levels_vec <-  unique(treatment_vec) %>% attr(.,"levels")
  }
  d = df %>% 
    mutate(i = -1,
           wts = ipw)
  
  # CATEGORICAL TREATMENT
  if(!is.numeric(treatment_vec)){
    # d0 = df %>% 
    #   mutate(i = 0,
    #          a = 0,
    #          y = NA,
    #          wts = ipw)
    # 
    # d1 = df %>% 
    #   mutate(i = 1,
    #          a = 1,
    #          y = NA,
    #          wts = ipw)
    d0 = df %>% 
      mutate(i = 0,
             a = levels_vec[1],
             y = NA,
             wts = ipw)
    
    d1 = df %>% 
      mutate(i = 1,
             a = levels_vec[2],
             y = NA,
             wts = ipw)
  }
  
  # NUMERIC TREATMENT
  if(is.numeric(treatment_vec)){
    
    d0 = df %>% 
      mutate(i = 0,
             a = mean(treatment_vec)%>% round(.,2),
             y = NA,
             wts = ipw)
    
    d1 = df %>% 
      mutate(i = 1,
             a = mean(treatment_vec) + 1 %>% round(.,2),
             y = NA,
             wts = ipw)
  }
  
  
  d_combined <- bind_rows(d,d0,d1)
  
  if(length(ipw) == nrow(df)){
    fit <- geepack::geeglm(as.formula(updated_formula), 
                           data = d_combined,
                           id = id,
                           weights = wts,corstr = "independence")
    
    
  }
  
  if(length(ipw) < nrow(df)){
    fit <- glm(as.formula(updated_formula), 
               data = d_combined)
  }
  
  d_combined$predicted_y = NA
  d_combined$predicted_y = predict(fit,d_combined)
  
  return(c(mean(d_combined$predicted_y[d_combined$i==-1]),
           mean(d_combined$predicted_y[d_combined$i==0]),
           mean(d_combined$predicted_y[d_combined$i==1]),
           mean(d_combined$predicted_y[d_combined$i==1])-
             mean(d_combined$predicted_y[d_combined$i==0])))
  
}