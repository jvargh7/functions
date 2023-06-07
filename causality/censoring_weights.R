censoring_weights = function(c_formula,censoring_var_yes = TRUE,df,standardized=TRUE,type = "glmm",cluster_var = "d_id_unim",
                             n_formula = NULL){
  
  # censoring_var_yes: Is censoring_var == 1 --> Censoring? TRUE or FALSE
  
  # c_formula : CENSORING (YES = 1) ~ TREATMENT + COVARIATES
  # A --> C <-- L --> Y; A to be measured before C
  # If Pr[C = 1|A,L] = 0.4, then 2 out 5 such individuals do not have data
  # This means the 3 individuals that have the data need to account for the other 2
  # So their weight has to be 1/(1-0.4)
  
  
  censoring_var = str_split(c_formula," ~ ")[[1]][1]
  
  if(is.null(n_formula)){
    n_formula = str_split(c_formula," \\+ ")[[1]][1]
  }
  
  if(type == "glm"){
    d_glm <- glm(as.formula(c_formula),family="binomial",data=df)
    n_glm <- glm(as.formula(n_formula),family="binomial",data=df)
    

    
  }
  
  if(type == "glmm"){
    d_glm <- glmer(as.formula(paste0(c_formula," + (1|",cluster_var,")")),family="binomial",data=df)
    n_glm <- glmer(as.formula(paste0(n_formula," + (1|",cluster_var,")")),family="binomial",data=df)

  }
  
  if(censoring_var_yes == TRUE){
    p_d <- 1- predict(d_glm,type="response")
    p_n <- 1 - predict(n_glm,type="response")
    
  }else{
    p_d <- predict(d_glm,type="response")
    p_n <- predict(n_glm,type="response")
    
  }
  
  
  if(standardized==TRUE){
    w_c = p_n/p_d
  }
  if(standardized==FALSE){
    w_c = 1/p_d
  }
  
  return(w_c)
  
}