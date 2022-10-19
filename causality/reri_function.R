###############################################################################################################
# Program: RERI and CI's                                                                                      #
# Author: Natalie Levy                                                                                        #
# Date: August 19, 2020                                                                                       #
#                                                                                                             #
# Adapted from:                                                                                               #
# Mathur MB & VanderWeele TJ (2018). R function for additive interaction measures. Epidemiology 29(1), e5-e6. #
###############################################################################################################
##### The code below can be used with any multiplicative model from which RERI can be estimated (logistic, relative risk, complex survey)
##### For the code to work, the two interaction variable must be specified first in the model i.e. Y ~ exp1*exp2 + covariates
##### Additionally, the code assumes that variables are coded such that all effects are causal (>1)
##### The RERI_function takes a model object as its input. The desired CI level can also be specified


# Updated the RERI_function() with the following:
# 1. Pass exposure, effect modifier variable names
# 2. Return SE for multiple imputation

reri_function <- function(model, ci = 0.95,exposure = character(),modifier = character()) {
  
  names_nohet <- attr(model$coefficients,"names")
  
  
  ## Create variables to store coefficient names, common across all models we ran
  exposure1 <- exposure
  exposure2 <- modifier
  interaction.string <- paste(exposure1, exposure2, sep=":") 
  keepers <- c(exposure1, exposure2, interaction.string)
  
  ## Restrict variance-covariance matrix to just needed variables
  V <- as.matrix(vcov(model))
  V2 <- V[keepers, keepers]  
  
  # Calculate various coefficients and measures of effect
  # Note that although the variables below are labeled as OR, these can also be RR measures
  b10 <- coef(model)[exposure1]
  b01 <- coef(model)[exposure2]
  bint <- coef(model)[interaction.string]
  OR00 <- 1.0  # reference
  OR10 <- exp(b10)
  OR01 <- exp(b01)
  OR11 <- exp(b10 + b01 + bint)
  
  # Check that both exposures are causal, i.e. ORs or RRs > 1
  if(OR10 < 1 | OR01 < 1){
    stop ("Both exposures need to be coded such that they are causal. Please recode and then rerun the function.")
  }
  
  # RERI (VanderWeele pg. 258-259)
  RERI <- OR11 - OR10 - OR01 + 1
  
  # Standard error and 95% CI's for RERI using delta method
  require(msm)
  SE.RERI <- deltamethod( ~ exp(x1 + x2 + x3) - exp(x1) - exp(x2) + 1,
                          mean=c(coef(model)[exposure1], coef(model)[exposure2], coef(model)[interaction.string]), 
                          cov=V2)
  
  alpha <- 1-ci
  z <- qnorm(1 - alpha/2)  # critical value
  
  RERI.CL_L <- RERI - z*SE.RERI
  RERI.CL_U <- RERI + z*SE.RERI
  
  RERI.output <- c(RERI, RERI.CL_L, RERI.CL_U,SE.RERI)
  names(RERI.output) <- c("RERI", "Lower CI", "Upper CI","SE")
  return(RERI.output)
}