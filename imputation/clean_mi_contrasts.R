source("C:/code/external/functions/imputation/contrasts_lm.R")
source("C:/code/external/functions/imputation/contrasts_geeglm.R")
source("C:/code/external/functions/preprocessing/round_d.R")

clean_mi_contrasts <- function(model_list,link = "geeglm identity",model_matrix = NULL,vcov_type="robust",
                               modifier = character(),exposure = character(),exposure_value = 1, modifier_value = 1,e_m_term = TRUE){
  
  D = length(model_list)
  
  if(link %in% c("geeglm identity","lmer identity","lm","geeglm log")){
    
    df = purrr::imap_dfr(model_list ,
                         function(x,name) {
                           if(class(x)[1] == "lm"){
                             contrasts_out = contrasts_lm(fit=x,model_matrix=model_matrix,vcov_type = vcov_type,modifier=modifier,exposure=exposure,
                                                          exposure_value = exposure_value, modifier_value = modifier_value,e_m_term = e_m_term) %>% 
                               mutate(index = name)
                           }
                           if(class(x)[1] == "geeglm"){
                             contrasts_out = contrasts_geeglm(fit=x,model_matrix=model_matrix,vcov_type = vcov_type,modifier=modifier,exposure=exposure,
                                                              exposure_value = exposure_value, modifier_value = modifier_value,e_m_term = e_m_term) %>% 
                               mutate(index = name)
                           }
                           return(contrasts_out)
                         }) %>% 
      dplyr::filter(!is.na(SE)) %>% 
      mutate(W_d = SE^2) %>% 
      group_by(term) %>% 
      mutate(B_D = var(Estimate)) %>% 
      dplyr::summarize(B_D = mean(B_D), # B: Variance of estimates (between imputation variance)
                       W_D = mean(W_d), #\bar{V}: average of V_d over D imputed datasets
                       theta_D = mean(Estimate), #\bar{\theta}: mean of estimates,
                       dfcom = mean(dfcom)
      ) %>% 
      ungroup() %>% 
      
      mutate(T_D = W_D + (1 + 1/D)*B_D, # Var(\theta|Y_{0}) ~ improved approximation of posterior variance [\bar{V} + B] 
             gamma_D = (1 + 1/D)*(B_D/T_D), # \hat{\gamma}_D = between imputation : total variance --> fraction of missing information
             nu = (D-1)*((1+ (1/(D+1))*(W_D/B_D))^2), # degrees of freedom of t-distribution
             nu2 = (D-1)/(gamma_D)^2, # equivalent to mice:::pool.fitlist >> mice:::barnard.rubin()'s dfold; (D/(D+1)) and not (1/(D+1))
             nu_improved = mice:::barnard.rubin(D,B_D,T_D,dfcom = dfcom) 
      ) %>% 
      mutate(L = theta_D + qt(p = 0.025,df = nu2)*((T_D)^((1/2))),
             U = theta_D + qt(p = 0.975,df = nu2)*((T_D)^((1/2))),
             sqrt_T_D = ((T_D)^((1/2)))
      ) 
    if(link %in% c("lm","lmer identity","geeglm identity","lm_robust","lmer robust")){ 
    df = df %>%
      mutate(Coefficient = paste0(round_d(theta_D,2)," \t(",
                                  round_d(L,2),", ",
                                  round_d(U,2),")"),
             lci = L,
             uci = U
             
      ) %>% 
      rename(iv = term) 
    }
    
    
    if(link %in% c("glmer logit","glmer log","geeglm log")){
      df <- df %>%
        mutate(RR = paste0(round_d(exp(theta_D),2)," \t (",
                           round_d(exp(L),2),", ",
                           round_d(exp(U),2),")"),
               lci = exp(L),
               uci = exp(U)
               
        ) %>% 
        rename(iv = term) 
      
    }
    
  }
  
  return(df)
  
  
}

