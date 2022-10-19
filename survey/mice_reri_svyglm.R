
# Similar structure as "imputation/clean_mi_contrasts.R"

source("C:/code/external/functions/causality/reri_function.R")
# https://github.com/jvargh7/functions/blob/main/causality/reri_function.R

mice_reri_svyglm <- function(svymodel_list,modifier,exposure,estimand="RR"){
  
  D = length(svymodel_list)
  
  df = purrr::imap_dfr(svymodel_list ,
                       function(x,name) {
                         reri_out =reri_function(model = x,modifier = modifier,exposure=exposure) %>% 
                           mutate(index = name) 
                         
                         return(reri_out)
                       }) %>% 
    dplyr::filter(!is.na(SE)) %>% 
    mutate(W_d = SE^2) %>% 
    group_by(term) %>% 
    mutate(B_D = var(RERI)) %>% 
    dplyr::summarize(B_D = mean(B_D), # B: Variance of estimates (between imputation variance)
                     W_D = mean(W_d), #\bar{V}: average of V_d over D imputed datasets
                     theta_D = mean(RERI), #\bar{\theta}: mean of estimates,
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

    df <- df %>%
      mutate(RERI_CI = paste0(round_d(theta_D,2)," \t(",
                                  round_d(L,2),", ",
                                  round_d(U,2),")"),
             lci = L,
             uci = U) 
       
  
  
  return(df)
  
  
}