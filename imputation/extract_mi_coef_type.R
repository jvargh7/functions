extract_mi_coef_type = function(models_list, coef_name = NULL,coef_type = "Coefficient"){
  if(coef_type == "theta_D"){
    out_est = clean_mi_conditionalregression(models_list,link = "lmer identity") %>% 
      mutate(sex = "Combined") %>% 
      dplyr::filter(iv == coef_name) %>% 
      dplyr::select(theta_D) %>% 
      pull()
  }
  
  
  
  if(coef_type == "lci"){
    out_est = clean_mi_conditionalregression(models_list,link = "lmer identity") %>% 
      mutate(sex = "Combined") %>% 
      dplyr::filter(iv == coef_name) %>% 
      dplyr::select(lci) %>% 
      pull()
  }
  
  if(coef_type == "uci"){
    out_est = clean_mi_conditionalregression(models_list,link = "lmer identity") %>% 
      mutate(sex = "Combined") %>% 
      dplyr::filter(iv == coef_name) %>% 
      dplyr::select(uci) %>% 
      pull()
  }
  if(coef_type == "sqrt_T_D"){
    out_est = clean_mi_conditionalregression(models_list,link = "lmer identity") %>% 
      mutate(sex = "Combined") %>% 
      dplyr::filter(iv == coef_name) %>% 
      dplyr::select(sqrt_T_D) %>% 
      pull()
  }
  
  
  if(coef_type == "Coefficient"){
    out_est = clean_mi_conditionalregression(models_list,link = "lmer identity") %>% 
      mutate(sex = "Combined") %>% 
      dplyr::filter(iv == coef_name) %>% 
      dplyr::select(Coefficient) %>% 
      pull()
  }
  
  
  if(coef_type == "beta + se"){
    out_est = c(
      clean_mi_conditionalregression(models_list,link = "lmer identity") %>% 
        mutate(sex = "Combined") %>% 
        dplyr::filter(iv == coef_name) %>% 
        dplyr::select(theta_D) %>% 
        pull(),
      clean_mi_conditionalregression(models_list,link = "lmer identity") %>% 
        mutate(sex = "Combined") %>% 
        dplyr::filter(iv == coef_name) %>% 
        dplyr::select(sqrt_T_D) %>% 
        pull()
    )
  }
  
  return(out_est)
  
}