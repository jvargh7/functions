
save_geeglm <- function(model_fit){
  list(coefficients = coef(model_fit),
       corstr = model_fit$modelInfo$corstr,
       cor_link = model_fit$modelInfo$cor.link,
       mean_link = model_fit$modelInfo$mean.link,
       variance_link = model_fit$modelInfo$variance,
       residuals = model_fit$residuals,
       weights = model_fit$weights,
       df.residual = model_fit$df.residual,
       rank = model_fit$rank,
       xnames = model_fit$geese$xnames,
       naive.cov = model_fit$geese$vbeta.naiv,
       robust.cov = model_fit$geese$vbeta) %>% 
    return(.)
  
  
}

save_mi_geeglm <- function(model_list){

  map(model_list,
      function(fit){
        save_geeglm(fit) %>% 
          return(.)
        
      }) %>% 
    return(.)
  
  
    
    

}
