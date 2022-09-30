prepare_contrasts <- function(glm_nohet = NULL,glm_het = NULL,modifier = character(),exposure = character()){
  
  contrast_matrix_nohet <- NA
  contrast_matrix_het <- NA
  
  if(!is.null(glm_nohet)){
    nterms_nohet <- length(glm_nohet$coefficients)
    names_nohet <- attr(glm_nohet$coefficients,"names")
    
    
    contrast_matrix_nohet = matrix(c(
      rep(c(0),each=nterms_nohet),
      rep(c(0),each=nterms_nohet)
    ),
    nrow=2,
    byrow=TRUE
    )
    
    # Modifier = 1: Fixed effect
    contrast_matrix_nohet[1,which(names_nohet %in% modifier)] <- 1
    
    # Exposure: Fixed effect
    contrast_matrix_nohet[2,which(names_nohet %in% exposure)] <- 1
  }
  
  
  
  if(!is.null(glm_het)){
    nterms_het <- length(glm_het$coefficients)
    names_het <- attr(glm_het$coefficients,"names")
    
    contrast_matrix_het = matrix(c(
      rep(c(0),each=nterms_het),
      rep(c(0),each=nterms_het),
      rep(c(0),each=nterms_het)
    ),
    nrow=3,
    byrow=TRUE
    )
    
    # Exposure in Modifier = 0
    contrast_matrix_het[1,which(names_het %in% exposure)] <- 1
    
    # Exposure in Modifier = 1
    contrast_matrix_het[2,which(names_het %in% c(exposure,paste0(exposure,":",modifier)))] <- 1
    
    # Interaction effect of Exposure x Modifier
    contrast_matrix_het[3,which(names_het %in% c(paste0(exposure,":",modifier)))] <- 1
  }
  
  
  return(list(contrast_matrix_nohet,
              contrast_matrix_het))
  
}