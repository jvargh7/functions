adjusted_ci_path = function(model_list){
  D = length(model_list)
  
  df = purrr::imap_dfr(model_list ,
                       function(x,name) {
                         x %>%
                           broom::tidy(.) %>% 
                           mutate(index = name,
                                  dfcom = fit@SampleStats@ntotal[[1]])
                       })
  
  df = df %>% 
    dplyr::filter(!is.na(std.error),op=="~") %>% 
    mutate(W_d = std.error^2) %>% 
    group_by(term) %>% 
    mutate(B_D = var(estimate)) %>% 
    summarize(B_D = mean(B_D),
              W_D = mean(W_d),
              theta_D = mean(estimate),
              dfcom = mean(dfcom)
    ) %>% 
    ungroup() %>% 
    
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