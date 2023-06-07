# https://psyarxiv.com/d459g/ explains pooling methods for X2 and LRT
# https://cran.r-project.org/web/packages/psfmi/psfmi.pdf provides methods for D2 (X2) and D4 (LRT) pooling of mice glm objects
# Pending ------
qic_geeglm <- function(model_list){
  
  k <- length(coef(model_list[[1]]))
  
  QICs = map(model_list,
             function(x){
               QIC(x)
               
             }) %>% 
    as.numeric()
  
  m = length(model_list)
  # r3 =
  
  
  # d3 = mean(QICs)/()
  
  
  
}