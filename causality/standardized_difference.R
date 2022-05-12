# Austin 2015 Statist Med 34 (3661-3679)

wtd_var <- function(x,w){
  
  w_n = sum(w)
  w_d = sum(w)^2 - sum(w^2)
  
  mean_x = Hmisc::wtd.mean(x,w)
  
  x_n = sum(w*((x-mean_x)^2))
  
  var_x = (w_n*x_n)/w_d
  
  return(var_x)
  
}

standardized_difference <- function(a_variable,x_variable,weight_variable){
  a_levels = unique(a_variable)
  
  a_0 = a_levels[[1]]
  a_1 = a_levels[[2]]
  
  x_a0 = x_variable[a_variable == a_0]
  x_a1 = x_variable[a_variable == a_1]
  
  w_a0 = w_variable[a_variable == a_0]
  w_a1 = w_variable[a_variable == a_1]
  
  if(length(levels(x_0)) > 2){
    mean_x_1 = Hmisc::wtd.mean(x_a1,w_a1)
    mean_x_0 = Hmisc::wtd.mean(x_a0,w_a0)
    
    var_x_a0 = wtd_var(x_a0,w_a0)
    var_x_a1 = wtd_var(x_a1,w_a1)
    
    
    d = 100*(mean_x_a1 - mean_x_a0)/sqrt(0.5*(var_x_a1 + var_x_a0))
  }
  
  if(length(levels(x_0)) == 2){
    
    x_levels = unique(x_0)
    x_1 = x_levels[[1]]
    x_2 = x_levels[[2]]
    
    
    p_x_a0 = Hmisc::wtd.mean((x_a0 == x_1),weights = w_a0)
    p_x_a1 = Hmisc::wtd.mean((x_a1 == x_1),weights = w_a1)
    
    
    d = 100*((p_x_a1 - p_x_a0)/sqrt(0.5*(p_x_a1*(1-p_x_a1) + p_x_a0*(1-p_x_a0))))
    
  }
  
  return(d)
  
  
  
  
  
  
  

}