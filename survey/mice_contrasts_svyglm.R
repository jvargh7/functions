# Similar structure as "imputation/clean_mi_contrasts.R"

source("C:/code/external/functions/preprocessing/prepare_contrasts.R")
source("C:/code/external/functions/survey/contrasts_svyglm.R")
source("C:/code/external/functions/preprocessing/round_d.R")

# estimand: RR, OR, Beta
mice_contrasts_svyglm <- function(svymodel_list,modifier,exposure,estimand="RR"){
  
  D = length(svymodel_list)
  
  df = purrr::imap_dfr(svymodel_list ,
                       function(x,name) {
                         contrasts_out = prepare_contrasts(glm_het = x,modifier = modifier,exposure=exposure) %>% 
                           .[[2]] %>% 
                           # vcov_type is not really used since svyglm computes robust SE by default
                           contrasts_svyglm(model_matrix=.,fit=x) %>% 
                           mutate(index = name) 
                         
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
    ) %>% 
    mutate(term = case_when(term == "Contrast 1" ~ paste0(exposure," at ",modifier, "=",0),
                            term == "Contrast 2" ~ paste0(exposure," at ",modifier, "=",1),
                            term == "Contrast 3" ~ paste0("Interaction of ",exposure,":",modifier)))
  
  if(estimand == "Beta"){
    df <- df %>%
      mutate(Coefficient = paste0(round_d(theta_D,2)," \t(",
                                  round_d(L,2),", ",
                                  round_d(U,2),")"),
             lci = L,
             uci = U) 
  }
  
  if(estimand %in% c("RR","PR","OR")){
    df <- df %>%
      mutate(RR = paste0(round_d(exp(theta_D),2)," \t(",
                         round_d(exp(L),2),", ",
                         round_d(exp(U),2),")"),
             lci = exp(L),
             uci = exp(U)
             
      )
  }       
    
  
  return(df)
  
  
}