source("C:/code/external/functions/preprocessing/prepare_contrasts.R")

contrasts_glmer <- function(fit,model_matrix = NULL,row_names = NULL,
                         modifier = NULL,
                         exposure = NULL,vcov_type = "naive",
                         exposure_value = 1,
                         modifier_value = 1){
  
  # fit = models_list[[1]]
  
  if(is.null(model_matrix)){
    model_matrix = prepare_contrasts(glm_het = fit,modifier = modifier,exposure=exposure,
                                     exposure_value = exposure_value, modifier_value = modifier_value) %>% 
      .[[2]]
  }
  
  vcov_lmer = vcov(fit)
  
  contrast_est = fixef(fit)%*%t(model_matrix)
  contrast_se = sqrt(model_matrix%*%vcov_lmer%*% t(model_matrix))
  
  output = data.frame(Estimate = contrast_est[1,],
                      SE = diag(contrast_se)
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
