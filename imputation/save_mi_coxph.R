save_coxph <- function(model_fit){
  list(coefficients = coef(model_fit),
       corstr = NULL,
       cor_link = NULL,
       mean_link = NULL,
       variance_link = NULL,
       residuals = model_fit$residuals,
       weights = model_fit$weights,
       ntotal = model_fit$n,
       nevent = model_fit$nevent,
       rank = NULL,
       xnames = NULL,
       naive.cov = model_fit$naive.var,
       robust.cov = model_fit$var) %>% 
    return(.)
  
  
}

save_mi_coxph <- function(model_list){
  
  map(model_list,
      function(fit){
        save_coxph(fit) %>% 
          return(.)
        
      }) %>% 
    return(.)
  
  
  
  
  
}