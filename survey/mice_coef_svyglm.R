
# adjusted_ci:  Used for pooling 
source("C:/code/external/functions/imputation/adjusted_ci.R")
# round_d: Rounds values to n decimals as a sprintf() wrapper around round()
source("C:/code/external/functions/preprocessing/round_d.R")

mice_coef_svyglm <- function(model_list,link="svyglm quasipoisson"){

  if(link %in% c("svyglm quasipoisson")){
    res_out <- adjusted_ci(model_list,link) %>%
      mutate(RR = paste0(round_d(exp(theta_D),2)," (",
                                  round_d(exp(L),2),", ",
                                  round_d(exp(U),2),")"),
             lci = exp(L),
             uci = exp(U)
             
      ) %>% 
      rename(iv = term) 
  }
  
  if(link %in% c("svyglm gaussian")){
    res_out <- adjusted_ci(model_list,link) %>%
      mutate(Coefficient = paste0(round_d(theta_D,2)," (",
                                  round_d(L,2),", ",
                                  round_d(U,2),")"),
             lci = L,
             uci = U
             
      ) %>% 
      rename(iv = term) 
  }
  
  return(res_out)
  
}