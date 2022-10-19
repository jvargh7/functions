lm_wald_power <- function(X_mat,y_col,alpha_2sided = 0.05,
                                n=50,
                                coefficient_index = 2){
  
  # Refer to BIOS 525 > Lecture 5 > Slide 8
  # n = sample size to test
  # coefficient_index = index in design matrix (X_mat) for coefficient of interest
  
  # b0 = 0,b1 = 0.4,sigma_sq = 0.2,
    # X_mat = matrix(
    #   c(rep(1,times=n),
    #     rnorm(n)),
    #   ncol=2,byrow=FALSE
    # )
    # y_col = X_mat%*%c(b0,b1) + rnorm(n,sd=sigma_sq)
    
    beta_hat = solve(t(X_mat)%*%X_mat)%*%t(X_mat)%*%y_col
    se_beta = sqrt(solve(t(X_mat)%*%X_mat))
    
    z_alphaBy2 = qnorm(alpha_2sided/2)
    
    # Wald test for beta_hat
    power = pnorm(z_alphaBy2 + abs(beta_hat[coefficient_index]/se_beta[coefficient_index,coefficient_index]))
    
    return(power)

}



