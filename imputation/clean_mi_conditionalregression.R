source("C:/code/external/functions/preprocessing/round_d.R")
source("C:/code/external/functions/imputation/adjusted_ci.R")

clean_mi_conditionalregression <- function(model_list,link = "lmer identity"){
  
  # lmer robust works for 2 level models: 
  # https://stackoverflow.com/questions/26412581/robust-standard-errors-for-mixed-effects-models-in-lme4-package-of-r
  if(link %in% c("lm","lmer identity","geeglm identity","lm_robust","lmer robust")){
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
  
  
  if(link %in% c("glmer logit","glmer log","geeglm log")){
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
