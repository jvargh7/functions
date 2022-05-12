source("C:/code/external/functions/imputation/round_d.R")

clean_mi_conditionalregression <- function(model_list,link = "lmer identity"){
  
  
  if(link %in% c("lm","lmer identity","geeglm identity")){
    res_out <- adjusted_ci(model_list,link) %>%
      mutate(Coefficient = paste0(round_d(theta_D,2)," \t(",
                                  round_d(L,2),", ",
                                  round_d(U,2),")"),
             lci = L,
             uci = U
             
      ) %>% 
      rename(iv = term) 
  }
  
  if(link %in% c("lm_robust")){
    res_out <- adjusted_ci(model_list,link) %>%
      mutate(Coefficient = paste0(round_d(theta_D,2)," \t(",
                                  round_d(L,2),", ",
                                  round_d(U,2),")"),
             lci = L,
             uci = U
             
      ) %>% 
      rename(iv = term) 
  }
  
  if(link %in% c("lm_robust sss")){
    res_out <- adjusted_ci(model_list,link)
  }
  
  
  if(link == "glmer logit"){
    res_out <- adjusted_ci(model_list,link) %>%
      mutate(OR = paste0(round_d(exp(theta_D),2)," \t (",
                         round_d(exp(L),2),", ",
                         round_d(exp(U),2),")"),
             lci = exp(L),
             uci = exp(U)
             
      ) %>% 
      rename(iv = term) 
  }
  
  if(link == "zinf glmmTMB log"){
    res_out <- adjusted_ci(model_list,link) %>% 
      # dplyr::filter(term != "(Intercept)",!is.na(std.error)) %>% 
      mutate(RR = paste0(round_d(exp(theta_D),2)," \t (",
                         round_d(exp(L),2),", ",
                         round_d(exp(U),2),")"),
             lci = exp(L),
             uci = exp(U)
             
      ) %>% 
      rename(iv = term)  
  }
  
  if(link == "parametric G"){
    res_out <- adjusted_ci(model_list,link) %>%
      mutate(Coefficient = paste0(round_d(theta_D,2)," \t(",
                                  round_d(L,2),", ",
                                  round_d(U,2),")"),
             lci = L,
             uci = U
             
      ) %>% 
      rename(iv = term)  
  }
  
  
  return(res_out)
  
}
