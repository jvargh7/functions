contrasts_lm <- function(fit,model_matrix,vcov_type = "robust"){
  
  # fit = models_list[[1]]
  
  if(vcov_type == "robust"){
    vcov_lm = sandwich::vcovHC(fit, "HC3")
  } else{vcov_lm = vcov(fit)}
  
  
  
  contrast_est = coef(fit)%*%t(model_matrix)
  contrast_se = sqrt(model_matrix%*%vcov_lm%*% t(model_matrix))
  
  output = data.frame(Estimate = contrast_est[1,],
                      SE = diag(contrast_se),
                      dfcom = fit$df.residual[[1]]
  ) %>% 
    mutate(LCI = Estimate - 1.96*SE,
           UCI = Estimate + 1.96*SE)
  
  output$term = paste0("Contrast ",c(1:nrow(output)))
  
  return(output)
  
  
  
}
