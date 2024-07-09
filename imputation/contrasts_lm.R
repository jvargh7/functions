source("C:/code/external/functions/preprocessing/prepare_contrasts.R")


contrasts_lm <- function(fit,model_matrix,row_names = NULL,
                         modifier = NULL,
                         exposure = NULL,vcov_type = "robust",
                         exposure_value = 1,
                         modifier_value = 1){
  
  # fit = models_list[[1]]
  
 
  if(is.null(model_matrix)){
    model_matrix = prepare_contrasts(glm_het = fit,modifier = modifier,exposure=exposure,
                                     exposure_value = exposure_value, modifier_value = modifier_value) %>% 
      .[[2]]
  }
  
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
  
  if(is.null(row_names)|length(row_names)!=nrow(output)){
    output$term = paste0("Contrast ",c(1:nrow(output)))
  }
  if(length(row_names)==nrow(output)){
    output$term = row_names
  }
  
  return(output)
  
  
  
}

