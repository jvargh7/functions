
if(Sys.info()["user"] =="JVARGH7"){
  source("C:/code/external/functions/preprocessing/prepare_contrasts.R")
  source("C:/code/external/functions/preprocessing/round_d.R")
} else{
  print("Please download prepare_contrasts.R and round_d.R from https://github.com/jvargh7/functions/preprocessing")
  
}


# https://github.com/jvargh7/functions/blob/main/preprocessing/prepare_contrasts.R
# https://github.com/jvargh7/functions/blob/main/preprocessing/round_d.R

contrasts_svyglm <- function(model_matrix = NULL,
                             fit = NULL,
                             row_names = NULL,
                             vcov_fit = NULL,
                             coef_fit = NULL,
                             df_fit = NULL,
                             svymodel = NULL,
                             modifier = NULL,
                             exposure = NULL){
  
  if(!is.null(fit)){
    vcov_svy = vcov(fit)
    coef_svy = coef(fit)
    df_svy = fit$df.residual
  }
  if(is.null(fit)){
    if(is.null(svymodel)){
      vcov_svy = vcov_fit
      coef_svy = coef_fit
      df_svy = df_fit
    }
    if(!is.null(svymodel)){
      
      vcov_svy = vcov(svymodel)
      coef_svy = coef(svymodel)
      df_svy = svymodel$df.residual
      
      model_matrix = prepare_contrasts(glm_het = svymodel,modifier = modifier,exposure=exposure) %>% 
        .[[2]]
      
    }
    
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
  
  if(!is.null(svymodel)){
    output = output %>% 
      mutate(term = case_when(term == "Contrast 1" ~ paste0(exposure," at ",modifier, "=",0),
                              term == "Contrast 2" ~ paste0(exposure," at ",modifier, "=",1),
                              term == "Contrast 3" ~ paste0("Interaction of ",exposure,":",modifier)))
    
    
  }
  
  
  return(output)
  
  
  
}
