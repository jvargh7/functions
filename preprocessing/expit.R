expit = function(mu,se){
  out = case_when(!is.na(se) ~ paste0(exp(mu) %>% round(.,2)," (",
                                      exp(mu + qnorm(0.025)*se) %>% round(.,2),", ",
                                      exp(mu + qnorm(0.975)*se) %>% round(.,2),")"),
                  TRUE ~ "Ref (1.00)")
  return(out)
}

inv_logit = function(log_odds){
  
  (exp(log_odds)/(1+exp(log_odds))) %>% 
    return(.)
  
}
