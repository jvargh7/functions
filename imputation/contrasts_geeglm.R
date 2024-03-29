source("C:/code/external/functions/preprocessing/prepare_contrasts.R")

contrasts_geeglm <- function(model_matrix = NULL,
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
                             vcov_gee = NULL,
                             coef_gee = NULL,
                             dfcom_gee = NULL){
  
  if(is.null(model_matrix)){
    model_matrix = prepare_contrasts(glm_het = fit,modifier = modifier,exposure=exposure,exposure_value = exposure_value, modifier_value = modifier_value,e_m_term = e_m_term) %>% 
      .[[2]]
  }
  
  
  if(is.null(vcov_gee)){
    vcov_gee = if(vcov_type =="robust"){
      fit$geese$vbeta}else{fit$geese$vbeta.naiv}
  }
  
  if(is.null(coef_gee)){
    coef_gee = coef(fit)
  }
  if(is.null(dfcom_gee)){
    dfcom_gee = fit$df.residual[[1]]
  }
 
  contrast_est = coef_gee%*%t(model_matrix)
  contrast_se = sqrt(model_matrix%*%vcov_gee%*% t(model_matrix))
  
  output = data.frame(Estimate = contrast_est[1,],
                      SE = diag(contrast_se),
                      dfcom = dfcom_gee) %>% 
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
