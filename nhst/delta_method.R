# https://cran.r-project.org/web/packages/modmarg/vignettes/delta-method.html

delta_method <- function(model,pred_df,weight_var = "combined_sampleweight"){
  
  pred_e = predict(model,newdata=pred_df,type="link")
  
  if(is.null(weight_var)){
    w = pred_df[,weight_var] %>% as.numeric()
  }else{
    # Present in geeglm and glm
    w = model$weights
  }

  
  m_family = family(model)
  
  
  if(m_family$family == "binomial" & m_family$link == "logit"){
    pred_e_response = 1 / (1 + exp(-pred_e))
    # mean_pred = Hmisc::wtd.mean(pred_e_response,weights = pred_df$combined_sampleweight,normwt = TRUE)
    mean_pred = Hmisc::wtd.mean(pred_e_response,weights = w,normwt = TRUE)
    deriv <- as.vector(exp(-pred_e) / (1 + exp(-pred_e))^2)
    
  } else if(m_family$link == "identity"){
    pred_e_response = pred_e
    # mean_pred = Hmisc::wtd.mean(pred_e_response,weights = pred_df$combined_sampleweight,normwt = TRUE)
    mean_pred = Hmisc::wtd.mean(pred_e_response,weights = w,normwt = TRUE)
    deriv <- rep(1,length(pred_e_response))
    }
  
  
  
  x = model.matrix(model)
  j <- deriv %*% x / nrow(x)
  variance <- j %*% vcov(model) %*% t(j)
  se_pred <- sqrt(diag(variance))
  
  return(list(mean = mean_pred,
         se = se_pred))
  
}