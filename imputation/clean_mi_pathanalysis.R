clean_mi_pathanalysis <- function(model_list){
  
  
  
  res_out <- adjusted_ci_path(model_list) %>%
    mutate(Coefficient = paste0(round(theta_D,2)," \t(",
                                round(L,2),", ",
                                round(U,2),")"),
           lci = L,
           uci = U
           
    ) %>% 
    separate(term,c("dv","iv")," ~ ")
  
  
  return(res_out)
  
}
