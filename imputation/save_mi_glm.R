
save_glm <- function(model_fit){
  list(coefficients = coef(model_fit),
       family = model_fit$family$family,
       link = model_fit$family$link,
       residuals = model_fit$residuals,
       weights = model_fit$weights,
       df.residual = model_fit$df.residual,
       rank = model_fit$rank,
       xnames = model_fit$xlevels,
       naive.cov = vcov(model_fit),
       robust.cov = sandwich::vcovHC(model_fit, "HC3")) %>% 
    return(.)
  
  
}

save_mi_glm <- function(model_list){

  map(model_list,
      function(fit){
        save_glm(fit) %>% 
          return(.)
        
      }) %>% 
    return(.)
  
  
    
    

}

tidy_save_glm <- function(glm_saved_fit,exponentiate = FALSE,vcov_type = "robust"){

coefs = glm_saved_fit$coefficients
if(vcov_type == "robust"){se = sqrt(diag(glm_saved_fit$robust.cov))}else{se = sqrt(diag(glm_saved_fit$naive.cov))}
term = names(coefs)

tidy_df = data.frame(
term = term,
estimate = coefs,
std.error = se)

if(exponentiate){
tidy_df = tidy_df %>%
mutate(coef = exp(estimate),
lci = exp(estimate - 1.96*std.error),
uci = exp(estimate + 1.96*std.error))
}else{
tidy_df = tidy_df %>%
mutate(
lci = estimate - 1.96*std.error,
uci = estimate + 1.96*std.error)
}

return(tidy_df)

}