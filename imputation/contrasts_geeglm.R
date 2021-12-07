contrasts_geeglm <- function(fit,model_matrix,vcov_type = "robust"){
  
  vcov_gee = if(vcov_type =="robust"){
    fit$geese$vbeta}else{fit$geese$vbeta.naiv}
  
  contrast_est = coef(fit)%*%t(model_matrix)
  contrast_se = sqrt(model_matrix%*%vcov_gee%*% t(model_matrix))
  
  output = data.frame(Estimate = contrast_est[1,],
                      SE = diag(contrast_se),
                      dfcom = fit$df.residual[[1]]) %>% 
    mutate(LCI = Estimate - 1.96*SE,
           UCI = Estimate + 1.96*SE)
  
  output$term = paste0("Contrast ",c(1:nrow(output)))
  
  return(output)
  
  
  
}
