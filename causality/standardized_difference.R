# Austin 2015 Statist Med 34 (3661-3679)

wtd_var <- function(x,w){
  
  w_n = sum(w)
  w_d = sum(w)^2 - sum(w^2)
  
  mean_x = Hmisc::wtd.mean(x,w)
  
  x_n = sum(w*((x-mean_x)^2))
  
  var_x = (w_n*x_n)/w_d
  
  return(var_x)
  
}

standardized_difference <- function(a_variable,x_variable,w_variable){
  a_levels = unique(a_variable)
  
  a_0 = a_levels[[1]]
  a_1 = a_levels[[2]]
  
  x_a0 = x_variable[a_variable == a_0]
  x_a1 = x_variable[a_variable == a_1]
  
  n_x_a0 = sum(!is.na(x_a0))
  n_x_a1 = sum(!is.na(x_a1))

  
  w_a0 = w_variable[a_variable == a_0]
  w_a1 = w_variable[a_variable == a_1]
  
  w_a0 = w_a0[!is.na(x_a0)]
  w_a1 = w_a1[!is.na(x_a1)]
  
  x_a0 = x_a0[!is.na(x_a0)]
  x_a1 = x_a1[!is.na(x_a1)]
  
  # Continuous variables
  if(length(unique(x_variable))>2 & class(x_variable)!="factor"){
    
	umean_x_a1 = mean(x_a1)
	umean_x_a0 = mean(x_a0)
	
	uvar_x_a0 = var(x_a0)
	uvar_x_a1 = var(x_a1)
	ud = 100*((umean_x_a1 - umean_x_a0)/sqrt(0.5*(uvar_x_a1 + uvar_x_a0)))

	mean_x_a1 = Hmisc::wtd.mean(x_a1,w_a1)
    mean_x_a0 = Hmisc::wtd.mean(x_a0,w_a0)
    
    var_x_a0 = wtd_var(x_a0,w_a0)
    var_x_a1 = wtd_var(x_a1,w_a1)
    
      d = 100*((mean_x_a1 - mean_x_a0)/sqrt(0.5*(var_x_a1 + var_x_a0)))

    output =  data.frame(
    umean_x_a1 = umean_x_a1,
  umean_x_a0 = umean_x_a0,  
  umean_diff = (umean_x_a1 - umean_x_a0),
  upooled_sd = sqrt(0.5*(uvar_x_a0 + uvar_x_a1)),	
	ud = ud,
  mean_x_a1 = mean_x_a1,
  mean_x_a0 = mean_x_a0,
  var_x_a1 = var_x_a1,
  var_x_a0 = var_x_a0,
  mean_diff = (mean_x_a1 - mean_x_a0),
  pooled_sd = sqrt(0.5*(var_x_a1 + var_x_a0)),
  smd = d)
  }
  
  # Binary variables or factors with 2 levels
  if(length(levels(x_variable)) == 2 | length(unique(x_variable)) == 2) {
    
    x_levels = unique(x_variable)
    x_1 = x_levels[[1]]
    x_2 = x_levels[[2]]
    
    umean_x_a0 = mean(x_a0==x_1)
	umean_x_a1 = mean(x_a1==x_1)
	
	uvar_x_a0 = umean_x_a0*(1-umean_x_a0)
    uvar_x_a1 = umean_x_a1*(1-umean_x_a1)
	ud = 100*((umean_x_a1 - umean_x_a0)/sqrt(0.5*(uvar_x_a1 + uvar_x_a0)))


    mean_x_a0 = Hmisc::wtd.mean((x_a0 == x_1),weights = w_a0)
    mean_x_a1 = Hmisc::wtd.mean((x_a1 == x_1),weights = w_a1)
	
	var_x_a0 = mean_x_a0*(1-mean_x_a0)
    var_x_a1 = mean_x_a1*(1-mean_x_a1)
    
    d = 100*((mean_x_a1 - mean_x_a0)/sqrt(0.5*(var_x_a1 + var_x_a0)))

    output =  data.frame(
  umean_x_a1 = umean_x_a1,
  umean_x_a0 = umean_x_a0,  
  umean_diff = (umean_x_a1 - umean_x_a0),
  upooled_sd = sqrt(0.5*(uvar_x_a0 + uvar_x_a1)),	
  ud = ud,
  mean_x_a1 = mean_x_a1,
  mean_x_a0 = mean_x_a0,
  var_x_a1 = var_x_a1,
  var_x_a0 = var_x_a0,
  mean_diff = (mean_x_a1 - mean_x_a0),
  pooled_sd = sqrt(0.5*(var_x_a1 + var_x_a0)),
  smd = d)
  }
  
  if(length(levels(x_variable)) > 2){
  
  x_levels = levels(x_variable)
  x_0 = x_levels[[1]]
  
  output = map_dfr(x_levels,
  .f = function(x_l){
  
    x_K = x_l
    
    umean_x_a0 = mean(x_a0==x_K)
	umean_x_a1 = mean(x_a1==x_K)
	
	uvar_x_a0 = umean_x_a0*(1-umean_x_a0)
    uvar_x_a1 = umean_x_a1*(1-umean_x_a1)
	
	ud = 100*((umean_x_a1 - umean_x_a0)/sqrt(0.5*(uvar_x_a1 + uvar_x_a0)))

	
    mean_x_a0 = Hmisc::wtd.mean((x_a0 == x_K),weights = w_a0)
    mean_x_a1 = Hmisc::wtd.mean((x_a1 == x_K),weights = w_a1)
	
	var_x_a0 = mean_x_a0*(1-mean_x_a0)
    var_x_a1 = mean_x_a1*(1-mean_x_a1)
    
    d = 100*((mean_x_a1 - mean_x_a0)/sqrt(0.5*(var_x_a1 + var_x_a0)))

    out_df =  data.frame(
    umean_x_a1 = umean_x_a1,
  umean_x_a0 = umean_x_a0,  
  umean_diff = (umean_x_a1 - umean_x_a0),
  upooled_sd = sqrt(0.5*(uvar_x_a0 + uvar_x_a1)),	
	ud = ud,	
  mean_x_a1 = mean_x_a1,
  mean_x_a0 = mean_x_a0,
  var_x_a1 = var_x_a1,
  var_x_a0 = var_x_a0,
  mean_diff = (mean_x_a1 - mean_x_a0),
  pooled_sd = sqrt(0.5*(var_x_a1 + var_x_a0)),
  smd = d)%>%
  mutate(level = x_l)
  
  return(out_df)
  
  }
  
  
  )

  
  
  }
  
  
  output %>%  
  return(.)
  
}