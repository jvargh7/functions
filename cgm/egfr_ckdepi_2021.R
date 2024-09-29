egfr_ckdepi_2021 = function(scr,female,age){
  
  vec1 = case_when(is.na(scr) ~ NA_real_,
                   female == 1 ~ scr/0.7,
                   TRUE ~ scr/0.9)
  
  min1 = pmin(vec1,1)^(case_when(female == 1 ~ -0.241,
                                 TRUE ~ -0.302))
  
  max2 = pmax(vec1,1)^(-1.200)
  
  age3 = (0.9938)^age
  
  coefsex4 = case_when(female == 1 ~ 1.012,
                       TRUE ~ 1)
  
  egfr_out = 142*min1*max2*age3*coefsex4
  
  return(egfr_out)
  
}
