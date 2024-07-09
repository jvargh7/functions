source("C:/code/external/functions/preprocessing/prepare_contrasts.R")


contrasts_glm <- function(model_matrix = NULL,
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
                             vcov_glm = NULL,
                             coef_glm = NULL,
                             dfcom_glm = NULL){
							 
						

 if(is.null(model_matrix)){
    model_matrix = prepare_contrasts(glm_het = fit,modifier = modifier,exposure=exposure,exposure_value = exposure_value, modifier_value = modifier_value,e_m_term = e_m_term) %>% 
      .[[2]]
  }
  
  
  if(is.null(vcov_glm)){
    if(vcov_type == "robust"){
    vcov_glm = sandwich::vcovHC(fit, "HC3")
  } else{vcov_glm = vcov(fit)}
  }
  
  if(is.null(coef_glm)){
    coef_glm = coef(fit)
  }
  if(is.null(dfcom_glm)){
    dfcom_glm = fit$df.residual
  }						
							 

  contrast_est = coef_glm%*%t(model_matrix)
  contrast_se = sqrt(model_matrix%*%vcov_glm%*% t(model_matrix))
  
  output = data.frame(Estimate = contrast_est[1,],
                      SE = diag(contrast_se),
                      dfcom = dfcom_glm
  ) %>% 
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

