source("C:/code/external/functions/preprocessing/prepare_contrasts.R")

contrasts_geeglm <- function(model_matrix = NULL,
                             fit = NULL,
                             row_names = NULL,
                             modifier = NULL,
                             exposure = NULL,
                             vcov_type = "robust",
                             exposure_value = 1,
                             modifier_value = 1,
                             e_m_term = TRUE){
  
  if(is.null(model_matrix)){
    model_matrix = prepare_contrasts(glm_het = fit,modifier = modifier,exposure=exposure,exposure_value = exposure_value, modifier_value = modifier_value,e_m_term = e_m_term) %>% 
      .[[2]]
  }
  
  
  
  vcov_gee = if(vcov_type =="robust"){
    fit$geese$vbeta}else{fit$geese$vbeta.naiv}
  
  contrast_est = coef(fit)%*%t(model_matrix)
  contrast_se = sqrt(model_matrix%*%vcov_gee%*% t(model_matrix))
  
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
