
# adjusted_ci: 
source("C:/code/external/functions/imputation/adjusted_ci.R")
# round_d
source("C:/code/external/functions/preprocessing/round_d.R")

mice_coef_svyglm <- function(model_list,link="svyglm quasipoisson"){

  if(link %in% c("svyglm quasipoisson")){
    res_out <- adjusted_ci(model_list,link) %>%
      mutate(RR = paste0(round_d(exp(theta_D),2)," \t(",
                                  round_d(exp(L),2),", ",
                                  round_d(exp(U),2),")"),
             lci = exp(L),
             uci = exp(U)
             
      ) %>% 
      rename(iv = term) 
  }
  
  return(res_out)
  
}