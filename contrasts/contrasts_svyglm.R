contrasts_svyglm <- function(fit,model_matrix,
                             row_names = NULL){
  
  vcov_svy = vcov(fit)
  
  contrast_est = coef(fit)%*%t(model_matrix)
  contrast_se = sqrt(model_matrix%*%vcov_svy%*% t(model_matrix))
  
  output = data.frame(Estimate = contrast_est[1,],
                      SE = diag(contrast_se),
                      dfcom = fit$df.residual[[1]]) %>% 
    mutate(LCI = Estimate - 1.96*SE,
           UCI = Estimate + 1.96*SE)
  
  
  if(is.null(row_names)|length(row_names)!=nrow(output)){
    output$term = paste0("Contrast ",c(1:nrow(output)))
  }
  if(length(row_names)==nrow(output)){
    output$term = row_names
  }
  
  return(output)
  
  
  
}
