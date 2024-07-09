# When using outcome standardization and to get average treatment effects among treated

att_estimation <- function(df_treated,model_obj,exposure_var = character(),weight_var = ""){

  
  pred_treated_treated = df_treated %>% 
    mutate(across(one_of(exposure_var), ~1)) %>% 
    predict(model_obj,newdata=.,type="response")
  
  pred_treated_untreated = df_treated %>% 
    mutate(across(one_of(exposure_var), ~0)) %>% 
    predict(model_obj,newdata=.,type="response")
  
  att = mean(pred_treated_treated) - mean(pred_treated_untreated)
  
  if(weight_var == ""){
    data.frame(ptt = mean(pred_treated_treated),
               ptu = mean(pred_treated_untreated),
               att = att) %>% 
      
      return(.)
  } else{
    
    data.frame(ptt = mean(pred_treated_treated),
               ptu = mean(pred_treated_untreated),
               weighted_ptt = Hmisc::wtd.mean(pred_treated_treated,weights=df_treated[,weight_var]),
               weighted_ptu = Hmisc::wtd.mean(pred_treated_untreated,weights=df_treated[,weight_var]),
               att = att) %>% 
      mutate(weighted_att = weighted_ptt - weighted_ptu) %>% 
      
      return(.)
  }
  
  

}

