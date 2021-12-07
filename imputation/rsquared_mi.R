rsquared_mi <- function(mice_obj, method = "mean",type="R2c"){
  
  if(type == "R2c"){
    rsq_obj <- purrr::map(mice_obj$analyses,function(x) r.squaredGLMM(x)[[2]]) %>% unlist()
  }
  if(type == "R2m"){
    rsq_obj <- purrr::map(mice_obj$analyses,function(x) r.squaredGLMM(x)[[1]]) %>% unlist()
  }
  
  if(method=="mean"){
    rsq_out <- mean(rsq_obj)
  }
  
  return(rsq_out)
}
