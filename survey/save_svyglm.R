save_svyglm <- function(svy_glm){
  
  list(coefficients = svy_glm$coefficients,
       residuals = svy_glm$coefficients,
       weights = svy_glm$weights,
       df.residual = svy_glm$df.residual,
       df.null = svy_glm$df.null,
       naive.cov = svy_glm$naive.cov,
       cov.unscaled = svy_glm$cov.unscaled) %>% 
    return(.)
  
}
