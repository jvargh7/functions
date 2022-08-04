contrasts_svyglm <- function(model_matrix,
                             fit,
                             row_names = NULL,
                             vcov_fit = NULL,
                             coef_fit = NULL,
                             df_fit = NULL){
  
  if(!is.null(fit)){
    vcov_svy = vcov(fit)
    coef_svy = coef(fit)
    df_svy = fit$df.residual
  }
  if(is.null(fit)){
    vcov_svy = vcov_fit
    coef_svy = coef_fit
    df_svy = df_fit
  }
  
  contrast_est = coef_svy%*%t(model_matrix)
  contrast_se = sqrt(model_matrix%*%vcov_svy%*% t(model_matrix))
  
  output = data.frame(Estimate = contrast_est[1,],
                      SE = diag(contrast_se),
                      dfcom = df_svy[[1]]) %>% 
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
