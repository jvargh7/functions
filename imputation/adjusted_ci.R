adjusted_ci = function(model_list,link="lmer identity"){
  D = length(model_list)
  
  if(link == "zinf glmmTMB log"){
    df = purrr::imap_dfr(model_list,
                         function(x,name){
                           dfcom = summary(x)$AICtab["df.resid"] %>% as.numeric();
                           bind_rows(summary(x)$coefficients$cond %>% 
                                       data.frame() %>% 
                                       mutate(term = row.names(.)) %>% 
                                       mutate(type = "Conditional"),
                                     summary(x)$coefficients$zi %>% 
                                       data.frame() %>% 
                                       mutate(term = row.names(.)) %>% 
                                       mutate(type = "Zero Inflation")) %>% 
                             mutate(index = name,
                                    dfcom = dfcom)
                         }
                         
    ) %>% 
      rename(estimate = Estimate,
             std.error = Std..Error)
  }
  
  if(link %in% c("lm","lmer identity","glmer logit","glmer log",
                 "geeglm identity","svyglm quasipoisson","geeglm log","svyglm gaussian")){
    df = purrr::imap_dfr(model_list ,
                         function(x,name) {
                           dfcom = x$df.residual[[1]];
                           
                           x %>% 
                             broom::tidy(exponentiate=FALSE) %>%  
                             mutate(index = name,
                                    dfcom = dfcom)
                         })  %>% 
      mutate(type = "Conditional")
    
  }
  
  if(link %in% c("lm_robust")){
    require(sandwich)
    require(lmtest)
    
    # Original code; however this doesn't account for clustering by maternal identifier in Guatemala and barangay in Philippines
    df = purrr::imap_dfr(model_list ,
                         function(x,name) {
                           dfcom = x$df.residual[[1]];
                           
                           coeftest(x,vcovHC(x,"HC3"))[,] %>%
                             data.frame() %>%
                             mutate(term = rownames(.)) %>%
                             rename(estimate = Estimate,
                                    std.error = 'Std..Error',
                                    statistic = 't.value',
                                    p.value = 'Pr...t..') %>%
                             mutate(index = name,
                                    dfcom = dfcom)
                         })  %>%
      mutate(type = "Conditional")
    
    
    
    
  }
  
  if(link %in% c("lm_robust sss")){
    require(sandwich)
    require(lmtest)
    source(paste0(path_sss_repo,"/package/robust functions.R"))
    
    df <- pool_robust(model_list) %>% 
      summary_robust() %>% 
      mutate(theta_D = estimate)
    
    return(df)
    
  }
  
  
  if(link %in% c("parametric G","doubly robust G")){
    df = purrr::imap_dfr(model_list ,
                         function(x,name) {
                           
                           dfcom = x$df.residual[[1]];
                           x %>% 
                             mutate(index = name,
                                    dfcom = dfcom)
                         }) %>% 
      mutate(type = "Conditional")
    
  }
  
  if(link %in% c("coxph")){
    df = purrr::imap_dfr(model_list ,
                         function(x,name) {
                           
                           ntotal = x$n[[1]];
                           nevent = x$nevent[[1]]
                           x %>% 
                             broom::tidy(exponentiate=FALSE) %>%  
                             mutate(index = name,
                                    dfcom = ntotal,
                                    nevent = nevent)
                         })  %>% 
      mutate(type = "Conditional")
    
  }
  
  
  if(link %in% c("lmer robust")){
    require(lme4)
    require(merDeriv)
    require(clubSandwich)
    # https://stackoverflow.com/questions/26412581/robust-standard-errors-for-mixed-effects-models-in-lme4-package-of-r
    df = purrr::imap_dfr(model_list ,
                         function(x,name) {
                           print(name);
                           dfcom = x@Gp[2];
                           merDeriv_SE <- sandwich(x, bread = bread(x, full = TRUE),
                                            mean = meat(x, level = 2)) %>% 
                             sqrt(diag(.));
                           
                           clubSandwich_SE <- vcovCR(x,cluster = x@frame$psu, type = "CR0") %>% 
                             sqrt(diag());
                           
                           wald_SE = sqrt(diag(vcov(x)));
                           est = fixef(x);
                           data.frame(
                                      term = names(est),
                                      estimate = est,
                                      std.error = merDeriv_SE,
                                      wald_std.error = wald_SE,
                                       
                                      ) %>%
                             mutate(index = name,
                                    dfcom = dfcom)
                         })  %>%
      mutate(type = "Fixed Effects")
    
    
    
    
  }
  
  
  
  df = df %>% 
    dplyr::filter(!is.na(std.error)) %>% 
    mutate(W_d = std.error^2) %>% 
    group_by(term,type) %>% 
    
    # Pages 233 - 235 of Little and Rubin 2019 Statistical Analysis with Missing Data
    mutate(B_D = var(estimate)) %>% 
    dplyr::summarize(B_D = mean(B_D),
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
