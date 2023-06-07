source("C:/code/external/functions/nhst/delta_method.R")

marginal_geeglm <- function(fit,margin_var = character(),margin_value = NULL,modifier_var = character(),modifier_value = NULL){
  
  if(!is.null(margin_value)){
    df = fit$model %>% 
      mutate_at(vars(margin_var),function(x) case_when(!is.na(x) ~ margin_value,
                                                       TRUE ~ as(NA,class(margin_value))))
    
    if(!is.null(modifier_var)){
      df = df %>% 
        mutate_at(vars(modifier_var),function(x) case_when(!is.na(x) ~ modifier_value,
                                                         TRUE ~ as(NA,class(modifier_value))))
      
    }
    
    
    delta_output = delta_method(model = fit,pred_df = df) %>% 
      data.frame()
    
  }
  
  return(delta_output)
}

# Test
# library(geeglm)
# data(dietox)
# dietox$Cu     <- as.factor(dietox$Cu)
# mf <- formula(Weight ~ Cu * (Time + I(Time^2) + I(Time^3)))
# gee1 <- geeglm(mf, data=dietox, id=Pig, family=poisson("identity"), corstr="ar1")
# gee1
# marginal_geeglm(gee1,"Cu","Cu035")

# dietox %>% 
#   mutate (n = nrow(.)) %>% 
#   group_by(Cu) %>% 
#   dplyr::summarize(m = mean(Weight),
#             s = sd(Weight)/sqrt(nrow(.)))
