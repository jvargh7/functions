save_mi_geeglm <- function(model_list){

  map(model_list,
      function(fit){
        
        list(coefficients = coef(fit),
             corstr = fit$modelInfo$corstr,
             cor_link = fit$modelInfo$cor.link,
             mean_link = fit$modelInfo$mean.link,
             variance_link = fit$modelInfo$variance,
             residuals = fit$residuals,
             weights = fit$weights,
             df.residual = fit$df.residual,
             rank = fit$rank,
             naive.cov = fit$geese$vbeta.naiv,
             robust.cov = fit$geese$vbeta) %>% 
          return(.)
        
      }) %>% 
    return(.)
  
  
    
    

}
