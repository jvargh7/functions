trim_probabilities <- function(probabilities,lower_limit_percentile = 0.025,upper_limit_percentile = 0.975){
  
    lower_limit_value = quantile(probabilities,lower_limit_percentile)
    upper_limit_value = quantile(probabilities,upper_limit_percentile)

  
  trimmed <- case_when(probabilities > upper_limit_value ~ upper_limit_value,
                       probabilities < lower_limit_value ~ lower_limit_value,
            TRUE ~ probabilities)
  
  return(trimmed)
  
}