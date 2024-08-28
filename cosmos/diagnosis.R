


diagnosis <- function(connection_Cosmos,year,project_string,like_dx = NA_character_,by_letters = FALSE,alphabets=NULL){
  
  # See aap_t2dm/atdat06_ for example of when by_letters are used
  if(by_letters){
    
    alphabets_string = alphabets %>%  as.character() %>% paste0(.,collapse="','") %>% paste0("'",.,"'")
    letters_filter = paste0("AND SUBSTRING(dtd.Value,1,1) IN (",alphabets_string,")")
    
  } else{letters_filter = ""}
  

  if(!is.na(like_dx[1])){
    like_string = like_dx %>%  as.character() %>% paste0(.,collapse="','") %>% paste0("'",.,"'")
    
    selected_dx <- paste0(" 
                        INNER JOIN (
                        SELECT * 
                        FROM(
                          SELECT dtd_internal2.DiagnosisKey, dtd_internal2.Type, dtd_internal2.Value, 
                          SUBSTRING(dtd_internal2.Value,1,3) AS Value_Grouper
                          FROM dbo.DiagnosisTerminologyDim dtd_internal2
                          ) dtd_internal
                        WHERE dtd_internal.Value_Grouper IN (",like_string,") 
                        ) dtd 
                        ON def.DiagnosisKey = dtd.DiagnosisKey
                        ")
    
    date_vars_all_dxcodes = "all_dxcodes.Year, all_dxcodes.YearMonth"
    date_vars_dd = "dd.Year, dd.YearMonth"
    
  }else{
    selected_dx = " INNER JOIN dbo.DiagnosisTerminologyDim dtd
	         ON def.DiagnosisKey = dtd.DiagnosisKey
          "
    date_vars_all_dxcodes = "all_dxcodes.Year"
    date_vars_dd = "dd.Year"
    
  }
  
  
  query <- paste0("SELECT all_dxcodes.PatientDurableKey, all_dxcodes.Value as ICD10_Value, ",date_vars_all_dxcodes,", 
  SUBSTRING(all_dxcodes.Value,1,3) AS Value_Grouper,
  SUBSTRING(all_dxcodes.Value,1,1) AS Value_Grouper2
  
  FROM(SELECT DISTINCT def.PatientDurableKey, dtd.Value, ",date_vars_dd,"
        FROM dbo.DiagnosisEventFact def
	        INNER JOIN (
            SELECT DISTINCT(cohort.PatientDurableKey) as PatientDurableKey
            FROM ",project_string," cohort 
          ) distinct_cohort
	         ON def.PatientDurableKey = distinct_cohort.PatientDurableKey
	        INNER JOIN dbo.DateDim dd
	         ON def.StartDateKey = dd.DateKey ",
          selected_dx,
        " WHERE (def.StartDateKey > ",year,"0000) AND (def.StartDateKey < ",year+1,"0000)
	      AND dtd.Type = 'ICD-10-CM' ",letters_filter," 
	      ) all_dxcodes
  ")
  
  dbGetQuery(connection_Cosmos,query) %>% 
    return(.)
  
}
