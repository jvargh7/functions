e_value_ci = function(beta,se,sd_y,type="eval_ci"){
  
  d = beta/sd_y
  se_d = se/sd_y
  
  rr_beta = exp(0.91*d)
  rr_lci = exp(0.91*d - 1.78*se_d)
  rr_uci = exp(0.91*d + 1.78*se_d)
  
  if(rr_beta < 1){
    rr_est = 1/rr_beta
    e_val_est = rr_est + sqrt(rr_est*(rr_est - 1))
    rr_uci_est = ifelse(rr_uci >= 1, rr_uci,1/rr_uci)
    # If UL >= 1, then E-value = 1
    # If UL < 1, then let UL* = 1/UL and E-value = UL* + sqrt{UL* × (UL* - 1)}
    e_val_ci = ifelse(rr_uci >= 1,1,rr_uci_est + sqrt(rr_uci_est*(rr_uci_est-1)))
  }
  
  
  if(rr_beta >= 1){
    rr_est = rr_beta
    e_val_est = rr_est + sqrt(rr_est*(rr_est - 1))
    
    e_val_ci = ifelse(rr_lci <= 1,1,rr_lci + sqrt(rr_lci*(rr_lci-1)))
    # If LL <= 1, then E-value = 1
    # If LL > 1, then E-value = LL + sqrt{LL × (LL - 1)}
  }
  
  
  
  output <-  list(paste0(round(e_val_est,2),"; CI: ",round(e_val_ci,2)),
                  round(e_val_est,3),
                  round(e_val_ci,3),
                  paste0(evalue_to_beta(e_val_est) %>% round(.,2),"; CI: ",
                         evalue_to_beta(e_val_ci) %>% round(.,2))
  )
  
  return(output)
  
}
