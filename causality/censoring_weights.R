censoring_weights = function(c_formula,df,standardized=TRUE,type = "glmm",cluster_var = "d_id_unim",
                             n_formula = NULL){
  
  # a_formula : CENSORING (YES = 1) ~ TREATMENT + COVARIATES
  censoring_var = str_split(c_formula," ~ ")[[1]][1]
  
  if(is.null(n_formula)){
    n_formula = str_split(c_formula," \\+ ")[[1]][1]
  }
  
  if(type == "glm"){
    d_glm <- glm(as.formula(c_formula),family="binomial",data=df)
    p_d <- 1- predict(d_glm,type="response")
    
    n_glm <- glm(as.formula(n_formula),family="binomial",data=df)
    p_n <- 1 - predict(n_glm,type="response")
  }
  
  if(type == "glmm"){
    d_glm <- glmer(as.formula(paste0(c_formula," + (1|",cluster_var,")")),family="binomial",data=df)
    p_d <- 1- predict(d_glm,type="response")
    
    n_glm <- glmer(as.formula(paste0(n_formula," + (1|",cluster_var,")")),family="binomial",data=df)
    p_n <- 1 - predict(n_glm,type="response")
    
    
  }
  
  if(standardized==TRUE){
    w_c = p_n/p_d
  }
  if(standardized==FALSE){
    w_c = 1/p_d
  }
  
  return(w_c)
  
}