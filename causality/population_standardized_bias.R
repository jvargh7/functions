
psb_return <- function(x_treatment_level, x_pooled,ipw_treatment_level,type = "c"){
  if(type %in% c("c","p")){
    
    xw_bar = Hmisc::wtd.mean(x = x_treatment_level,weights = ipw_treatment_level,normwt = TRUE)
    x_bar = mean(x_pooled)
    
    if(type == "c"){
      sd_hat = sd(x_pooled)
    } else{
      sd_hat = sqrt((x_bar*(1-x_bar)))
    }
    
    psb = abs(xw_bar - x_bar)/sd_hat
    
    psb_t_l = data.frame(xw_bar = xw_bar,
               x_bar = x_bar,
               sd_hat = sd_hat,
               psb = psb) 
    
  } else if(type == "g"){
    
    unique_x_levels = unique(x_pooled)
    
    psb_t_l = map_dfr(unique_x_levels,
            function(u_x_l){
              
              x_dummy = as.numeric(x_treatment_level==u_x_l)
              xw_bar = Hmisc::wtd.mean(x = x_dummy,weights = ipw_treatment_level,normwt = TRUE)
              x_bar = mean(x_dummy)
              sd_hat = sqrt((x_bar*(1-x_bar)))
              
              psb_u_x_l = abs(xw_bar - x_bar)/sd_hat
              
              data.frame(level = u_x_l,
                         xw_bar = xw_bar,
                         x_bar = x_bar,
                         sd_hat = sd_hat,
                         psb = psb_u_x_l) %>% 
                return(.)
              
            })
  
  }
  
  return(psb_t_l)
  
  
}




population_standardized_bias <- function(df = data.frame(),x_var,t_var,ipw_var,type = "c"){
  
  
  x = df %>% dplyr::select(x_var) %>% pull()
  t = df %>% dplyr::select(t_var) %>% pull()
  ipw = df %>% dplyr::select(ipw_var) %>% pull()
  
  t_levels = unique(t) %>% as.character()
  
  # t_l = "unexposed"
  psb_df = map_dfr(t_levels,
                   function(t_l){
                     
                     x_t_l = x[t==t_l]
                     ipw_t_l = ipw[t==t_l]
                     
                     p_r = psb_return(x_treatment_level = x_t_l,
                                      x_pooled = x,
                                      ipw_treatment_level = ipw_t_l,
                                      type = type) %>% 
                       mutate(treatment_level = t_l)
                     
                     return(p_r)
                     
                   }) %>% 
    mutate(variable = x_var)
  
  return(psb_df)
    
    
    
  }
