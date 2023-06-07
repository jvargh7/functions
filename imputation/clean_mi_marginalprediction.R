source("C:/code/external/functions/imputation/marginal_geeglm.R")

clean_mi_marginalprediction <- function(model_list,margin_var = character(),margin_value = NULL,
                                        modifier_var = NULL, modifier_value = NULL,
                                        link="geeglm identity"){
  
  if(link %in% c("geeglm identity")){
    
    df = purrr::imap_dfr(model_list ,
                         function(x,name) {
                           dfcom = x$df.residual[[1]];
                           
                           marginal_geeglm(x,margin_var = margin_var,margin_value = margin_value,
                                           modifier_var = modifier_var, modifier_value = modifier_value) %>%  
                             mutate(index = name,
                                    dfcom = dfcom)
                           
                           
                         }) 
  }
  
  
  # Borrowed from "functions/imputation/clean_mi_conditionalregression.R" --
  df = df %>% 
    dplyr::filter(!is.na(se)) %>% 
    mutate(W_d = se^2) %>% 
    # Pages 233 - 235 of Little and Rubin 2019 Statistical Analysis with Missing Data
    mutate(B_D = var(mean)) %>% 
    dplyr::summarize(B_D = mean(B_D),
                     W_D = mean(W_d),
                     D = max(index),
                     theta_D = mean(mean),
                     dfcom = mean(dfcom)
    ) %>% 
    mutate(T_D = W_D + (1 + 1/D)*B_D,
           gamma_D = (1 + 1/D)*(B_D/T_D),
           nu = (D-1)*((1+ (1/(D+1))*(W_D/B_D))^2),
           nu2 = (D-1)/(gamma_D)^2, # equivalent to mice's dfold; (D/(D+1)) and not (1/(D+1))
           nu_improved = mice:::barnard.rubin(D,B_D,T_D,dfcom = dfcom) 
    ) %>% 
    mutate(L = theta_D + qt(p = 0.025,df = nu2)*((T_D)^((1/2))),
           U = theta_D + qt(p = 0.975,df = nu2)*((T_D)^((1/2))),
           sqrt_T_D = ((T_D)^((1/2)))
    )
  
  return(df)
  
  
}
