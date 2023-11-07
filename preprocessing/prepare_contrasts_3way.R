
prepare_contrasts_3way <- function(glm_het_3way = NULL,
                                   exposure = character(),
                                   modifier1 = character(),
                                   modifier2 = character(),
                                   exposure_value = 1,
                                   modifier1_value = 1,
                                   modifier2_value = 1,
                                   e_m1_m2_term = TRUE,
                                   e_m1_term = TRUE,
                                   e_m2_term = TRUE,
                                   nterms_het = NULL,
                                   names_het = NULL,
                                   cov_het = NULL,
                                   names_cov_het = NULL){
  
  
  contrast_matrix_het <- NA
  if(!is.null(glm_het_3way)){
    if(class(glm_het_3way)[1] == "geeglm"){
      nterms_het <- length(coef(glm_het_3way))
      names_het <- attr(coef(glm_het_3way),"names")
      cov_het =  glm_het_3way$geese$vbeta
      names_cov_het = glm_het$geese$xnames
      
    } else if(class(glm_het_3way)[1] %in% c("glmerMod","lmerMod")){
      nterms_het <- length(fixef(glm_het_3way))
      names_het <- attr(fixef(glm_het_3way),"names")
      cov_het =  vcov.merMod(glm_het_3way)
      names_cov_het = rownames(cov_het)
      
    } else{
      nterms_het <- length(glm_het_3way$coefficients)
      names_het <- attr(glm_het_3way$coefficients,"names")
      cov_het = glm_het_3way$naive.cov
      names_cov_het = rownames(cov_het) 
    }
    
    
    
  } else{
    nterms_het <- nterms_het
    names_het <- names_het
    cov_het =  cov_het
    names_cov_het = names_cov_het
    
  }
  
  
  contrast_matrix_het = matrix(c(
    rep(c(0),each=nterms_het),
    rep(c(0),each=nterms_het),
    rep(c(0),each=nterms_het)
  ),
  nrow=3,
  byrow=TRUE
  )
  
  e_m1_m2_term_var = ifelse(e_m1_m2_term,paste0(exposure,":",modifier1,":",modifier2),paste0(modifier1,":",modifier2,":",exposure))
  e_m1_term_var = ifelse(e_m1_term,paste0(exposure,":",modifier1),paste0(modifier1,":",exposure))
  e_m2_term_var = ifelse(e_m2_term,paste0(exposure,":",modifier2),paste0(modifier2,":",exposure))
  
  # Exposure in Modifier1 = 0, Modifier2 = 0
  contrast_matrix_het[1,which(names_het %in% exposure)] <- exposure_value
  
  # Exposure in Modifier = m1, Modifier2 = m2
  contrast_matrix_het[2,which(names_het %in% c(exposure))] <- exposure_value
  contrast_matrix_het[2,which(names_het %in% c(e_m1_term_var))] <- exposure_value*modifier1_value
  contrast_matrix_het[2,which(names_het %in% c(e_m2_term_var))] <- exposure_value*modifier2_value
  contrast_matrix_het[2,which(names_het %in% c(e_m1_m2_term_var))] <- exposure_value*modifier1_value*modifier2_value
  
  # Difference between Contrast 1 and Contrast 2
  contrast_matrix_het[3,which(names_het %in% c(e_m1_term_var))] <- exposure_value*modifier1_value
  contrast_matrix_het[3,which(names_het %in% c(e_m2_term_var))] <- exposure_value*modifier2_value
  contrast_matrix_het[3,which(names_het %in% c(e_m1_m2_term_var))] <- exposure_value*modifier1_value*modifier2_value
  
  # New code ===========
  # Removing unidentified variables --------
  
  if(length(names_het[!names_het %in% names_cov_het])> 0){
    contrast_matrix_het = contrast_matrix_het[,-which(!(names_het %in% names_cov_het))]
  }
  
  return(contrast_matrix_het)
}




