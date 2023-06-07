# https://www.bmj.com/content/343/bmj.d2090


se_from_pvalue = function(beta_list,p_list,se_old_list = NULL){
  
  map(1:length(beta_list),
      function(i){
        beta = beta_list[i]
        p = p_list[i]
        if(!is.na(p)){
          z = -0.862 + sqrt(0.743 - 2.404*log(p))
          se_new = abs(beta/z)
        } else{se_new = se_old_list[i]}
        
        return(se_new)
      }) %>% 
    unlist() %>% 
    return(.)

}
