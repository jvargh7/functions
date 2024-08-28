
diagnosis_distinct_Range_index <- function(connection_Cosmos,project_string,IndexDateKey_Var="EventDateKey",
                                               LowerInterval = 2,UpperInterval = 0,filter_year = numeric(),
                                               by_letters = FALSE,alphabets=NULL,
                                               detection_string = "SUBSTRING(dtd.Value,1,1)"){
  
  # See aap_t2dm/atdat06_ for example of when by_letters are used
  if(by_letters){
    
    alphabets_string = alphabets %>%  as.character() %>% paste0(.,collapse="','") %>% paste0("'",.,"'")
    letters_filter = paste0("AND ",detection_string," IN (",alphabets_string,")")
    
  } else{letters_filter = ""}
  
  sign1 = case_when(LowerInterval <= 0 ~ " - ",
                    TRUE ~ "+")
  
  sign2 = case_when(UpperInterval <= 0 ~ " - ",
                    TRUE ~ "+")
  
  
  
  query <- paste0("SELECT all_dxcodes.PatientDurableKey, all_dxcodes.Value as ICD10_Value, 
  all_dxcodes.Year, all_dxcodes.YearMonth,
  SUBSTRING(all_dxcodes.Value,1,3) AS Value_Grouper,
  SUBSTRING(all_dxcodes.Value,1,1) AS Value_Grouper2
  
  FROM(SELECT DISTINCT def.PatientDurableKey, dtd.Value,dd.Year, dd.YearMonth
        FROM dbo.DiagnosisEventFact def
	        INNER JOIN ",project_string," cohort 
	          ON def.PatientDurableKey = cohort.PatientDurableKey
	        INNER JOIN dbo.DateDim dd
	         ON def.StartDateKey = dd.DateKey
	        INNER JOIN dbo.DiagnosisTerminologyDim dtd
	         ON def.DiagnosisKey = dtd.DiagnosisKey
        WHERE (def.StartDateKey > ",filter_year,"0000) AND (def.StartDateKey < ",filter_year+1,"0000) AND 
            (def.StartDateKey >= (cohort.",IndexDateKey_Var,sign1,abs(LowerInterval)*10000,")) AND (def.StartDateKey < (cohort.",
                  IndexDateKey_Var,sign2,abs(UpperInterval)*10000,"))
	      AND dtd.Type = 'ICD-10-CM' ",letters_filter," 
	      ) all_dxcodes
  ")
  
  dbGetQuery(connection_Cosmos,query) %>% 
    return(.)
  
}