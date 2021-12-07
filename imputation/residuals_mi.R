residuals_mi <- function(mice_obj, method = "mean"){
  
  res_obj <- purrr::map_dfc(mice_obj$analyses,function(x) residuals(x)) 
  
  
  # res2 <- purrr::map_dfc(mice_obj$analyses,function(x) predict(x))
  # compare_df <- data.frame(complete(mi_dfs,1),res2 = res2[,1],res = res_obj[,1])
  
  if(method=="mean"){
    res_out <- rowMeans(res_obj)
  }
  
  if(method=="identity"){
    res_out <- res_obj
    names(res_out) <- paste0("V",c(1:length(mice_obj$analyses)))
  }
  
  return(res_out)
}