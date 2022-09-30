# minimum detectable effect

lm_wald_mde <- function(power,alpha_2sided,n=250000){
  
  beta_by_se = (qnorm(power) + qnorm(1-(alpha_2sided/2)))*(2/sqrt(n))
  
  return(beta_by_se)
  
}