prop_wald_mde <- function(power,alpha_2sided,n=100000,p1 = 0.50){
  
  require(tidyverse)
  
  # step = which(n%%c(10^c(0:5)) > 0) - 1
  # Assumes equal sample sizes in each group
  
  beta_by_se = (qnorm(power) + qnorm(1-(alpha_2sided/2)))*(1/sqrt(n)) # (p2 - p1)/sqrt(p1(1-p1) + p2(1-p2))
  
  p_diff = purrr::map_dfr(seq(0.0001,0.01,by=0.0005),
               function(p_d){
                 
                 diff = (beta_by_se)^2 - ((p_d)^2/(p1*(1-p1) + (p1+p_d)*(1-p1-p_d)));
                 
                 data.frame(p_d=p_d,
                            diff = abs(diff)) %>% 
                 return(.)
               }) %>% 
    dplyr::filter(diff == min(diff))
  
  
  return(p_diff)
  
}

