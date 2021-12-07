evalue_to_beta <- function(e_value){
  
  beta_evalue = log(e_value)/0.91
  
  return(beta_evalue)
}