treatment_weights = function(a_formula = character(),df,standardized=TRUE,type = "glmm",cluster_var = "d_id_unim",cutoff = 3,strata_var = NA){
  # a_formula : TREATMENT (YES = 1) ~ COVARIATES
  treatment_var = str_split(a_formula," ~ ")[[1]][1]
  treatment_vec = df %>% 
    dplyr::select(treatment_var) %>% 
    pull() 
  
  levels_vec <-  unique(treatment_vec) %>% as.character()
  
  if(is.na(strata_var)){
    n_formula = paste0(treatment_var," ~ 1")
  }
  if(!is.na(strata_var)){
    n_formula = paste0(treatment_var," ~ ",strata_var)
  }
  
  
  if(type == "glm"){
    d_glm <- glm(as.formula(a_formula),family="binomial",data=df)
    p_d <- predict(d_glm,type="response")
    
    
    
    
    n_glm <- glm(as.formula(n_formula),family="binomial",data=df)
    p_n <- predict(n_glm,type="response")
  }
  
  
  if(type == "multinom"){
    
    d_glm <- nnet::multinom(as.formula(a_formula),data=df,trace=FALSE)
    p_d <- predict(d_glm,type="probs")
    print("Denominator complete")
    
    n_glm <- nnet::multinom(as.formula(n_formula),data=df,trace=FALSE)
    p_n <- predict(n_glm,type="probs")
  }
  
  
  
  if(type == "glmm"){
    d_glmer <- glmer(as.formula(paste0(a_formula," + (1|",cluster_var,")")),family="binomial",data=df)
    p_d <- predict(d_glmer,type="response")
    
    n_glmer <- glmer(as.formula(paste0(n_formula," + (1|",cluster_var,")")),family="binomial",data=df)
    p_n <- predict(n_glmer,type="response")
    
    
  }
  
  
  if(type == "lm"){
    d_glm <- lm(as.formula(a_formula),data=df)
    pred_d <- predict(d_glm, type = "response")
    p_d <- dnorm(treatment_vec, pred_d, summary(d_glm)$sigma)
    
    n_glm <- lm(as.formula(n_formula),data=df)
    pred_n <- predict(n_glm, type = "response")
    p_n <- dnorm(treatment_vec, pred_n, summary(n_glm)$sigma)
  }
  
  
  if(!type %in% c("multinom","lm")){ 
    if(standardized==TRUE){
      
      w_a = ifelse(df[,treatment_var] == levels_vec[1],
                   ((1-p_n)/(1-p_d)),
                   (p_n/p_d)
      )
      
    }
    if(standardized==FALSE){
      w_a = ifelse(df[,treatment_var] == levels_vec[1],
                   (1/(1-p_d)),
                   (1/p_d)
      )
    }
  }
  
  if(type == "lm"){
    if(standardized==TRUE){
      w_a = p_n/p_d
    }
    if(standardized == FALSE){
      w_a = 1/p_d
    }
    
  }
  
  if(type == "multinom"){
    if(standardized==TRUE){
      tx_n_p = map(1:nrow(p_n),function(x){p_n[x,treatment_vec[x]]}) %>% as.numeric()
      tx_d_p = map(1:nrow(p_d),function(x){p_d[x,treatment_vec[x]]}) %>% as.numeric()

      w_a = tx_n_p/tx_d_p
    }
    if(standardized==FALSE){

      tx_d_p = map(1:nrow(p_d),function(x){p_d[x,treatment_vec[x]]}) %>% as.numeric()
      w_a = 1/tx_d_p

    }
  }
  
  if(!is.na(cutoff)){
    w_a <- case_when(w_a > cutoff ~ cutoff,
                     TRUE ~ w_a)
  }
  
  return(w_a)
  
}

