source("nhst/p_val_format.R")

z_test <- function(mean1,mean2,se1,se2,output = "z_p"){
  
  z = (mean1 - mean2)/sqrt(sum(se1^2 + se2^2))
  p = pnorm(z,lower.tail = TRUE)
  
  if(output == "z"){
    return(round(z,digits=2))
  }
  
  if(output == "p"){
    return(p_val_format(p))
  }
  
  if(output == "z_p"){
    paste0(round(z,digits=2),", ",
           p_val_format(p))
    
  }
  
  
  
}