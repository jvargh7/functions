prepare_contrasts <- function(glm_nohet = NULL,glm_het = NULL,
                              modifier = character(),exposure = character(),
                              exposure_value = 1,
                              modifier_value = 1,
                              e_m_term = TRUE,
                              nterms_het = NULL,
                              names_het = NULL,
                              cov_het = NULL,
                              names_cov_het = NULL){
  
  contrast_matrix_nohet <- NA
  contrast_matrix_het <- NA
  
  if(!is.null(glm_nohet)){
    
    if(class(glm_nohet)[1] == "geeglm"){
      nterms_nohet <- length(coef(glm_nohet))
      names_nohet <- attr(coef(glm_nohet),"names")
      cov_nohet =  glm_nohet$geese$vbeta
      
    } else{
      nterms_nohet <- length(glm_nohet$coefficients)
      names_nohet <- attr(glm_nohet$coefficients,"names")
      cov_nohet = glm_nohet$naive.cov
    }
   
    
    
    contrast_matrix_nohet = matrix(c(
      rep(c(0),each=nterms_nohet),
      rep(c(0),each=nterms_nohet)
    ),
    nrow=2,
    byrow=TRUE
    )
    
    # Modifier = 1: Fixed effect
    contrast_matrix_nohet[1,which(names_nohet %in% modifier)] <- modifier_value
    
    # Exposure: Fixed effect
    contrast_matrix_nohet[2,which(names_nohet %in% exposure)] <- exposure_value
  }
  
  
  
  if(!is.null(glm_het) | !is.null(nterms_het)){
    if(!is.null(glm_het)){
      if(class(glm_het)[1] == "geeglm"){
        nterms_het = length(coef(glm_het))
        names_het = attr(coef(glm_het),"names")
        cov_het =  glm_het$geese$vbeta
        names_cov_het = glm_het$geese$xnames
        
      } else if(class(glm_het)[1] %in% c("glmerMod","lmerMod")){
        nterms_het = length(fixef(glm_het))
        names_het = attr(fixef(glm_het),"names")
        cov_het =  vcov.merMod(glm_het)
        names_cov_het = rownames(cov_het)
        
      } else if(class(glm_het)[1] %in% c("coxph")){
        nterms_het = length(glm_het$coefficients)
        names_het = attr(glm_het$coefficients,"names")
        cov_het = glm_het$var
        # Only for coxph -------
        names_cov_het = names_het
        
      } else if (class(glm_het)[1] %in% c("svyglm","glm","lm")){
        nterms_het = length(glm_het$coefficients)
        names_het = attr(glm_het$coefficients,"names")
        cov_het = glm_het$naive.cov
        names_cov_het = rownames(cov_het) 
      }
    } else {
      nterms_het = nterms_het
      names_het = names_het
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
    
    e_m_term_var = ifelse(e_m_term,paste0(exposure,":",modifier),paste0(modifier,":",exposure))
    
    # Exposure in Modifier = 0
    contrast_matrix_het[1,which(names_het %in% exposure)] <- exposure_value
    
    # Exposure in Modifier = 1
    contrast_matrix_het[2,which(names_het %in% c(exposure))] <- exposure_value
    contrast_matrix_het[2,which(names_het %in% c(e_m_term_var))] <- exposure_value*modifier_value
    
    # Difference between Contrast 1 and Contrast 2
    contrast_matrix_het[3,which(names_het %in% c(e_m_term_var))] <- exposure_value*modifier_value
    
    # New code ===========
    # Removing unidentified variables --------
    
    if(length(names_het[!names_het %in% names_cov_het])> 0){
      contrast_matrix_het = contrast_matrix_het[,-which(!(names_het %in% names_cov_het))]
    }
    
    
  }
  

  return(list(contrast_matrix_nohet,
              contrast_matrix_het))
  
}