source("C:/code/external/functions/preprocessing/prepare_contrasts.R")

contrasts_coxph <- function(model_matrix = NULL,
                             # Used to create the model_matrix
                             modifier = NULL,
                             exposure = NULL,
                             vcov_type = "robust",
                             exposure_value = 1,
                             modifier_value = 1,
                             e_m_term = TRUE,
                             
                             # If using a fit model 
                             fit = NULL,
                             row_names = NULL,
                             
                             
                             # If using parameters of a saved model directly
                             vcov_coxph = NULL,
                             coef_coxph = NULL,
                             dfcom_coxph = NULL){
  
  if(is.null(model_matrix)){
    model_matrix = prepare_contrasts(glm_het = fit,modifier = modifier,exposure=exposure,exposure_value = exposure_value, modifier_value = modifier_value,e_m_term = e_m_term) %>% 
      .[[2]]
  }
  
  
  if(is.null(vcov_coxph)){
    vcov_coxph = if(vcov_type =="robust"){
      fit$var}else{fit$naive.var}
  }
  
  if(is.null(coef_coxph)){
    coef_coxph = coef(fit)
  }
  if(is.null(dfcom_coxph)){
    dfcom_coxph = fit$n
  }
  
  contrast_est = coef_coxph%*%t(model_matrix)
  contrast_se = sqrt(model_matrix%*%vcov_coxph%*% t(model_matrix))
  
  output = data.frame(Estimate = contrast_est[1,],
                      SE = diag(contrast_se),
                      dfcom = dfcom_coxph) %>% 
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
