
# For each filter_year == year,
# Check if a diagnosis falls within the EventDateKey's [LookBackInterval, FollowUpInterval] range
# A PatientDurableKey may have more than one observation in the larger parquet file because the range falls over several calendar years
# Use distinct(PatientDurableKey, ICD10_Value) on the parquet file or group_by(PatientDurableKey) to check if any of the years have the code of interest


problemlist_start_YearMonth_index <- function(connection_Cosmos,project_string,IndexDateKey_Var="EventDateKey",
                                          LookBackInterval = 2,FollowUpInterval = 0,filter_year = numeric(),
                                          by_letters = FALSE,alphabets=NULL,
                                          detection_string = "SUBSTRING(dtd.Value,1,1)"){
  
  # See aap_t2dm/atdat06_ for example of when by_letters are used
  if(by_letters){
    
    alphabets_string = alphabets %>%  as.character() %>% paste0(.,collapse="','") %>% paste0("'",.,"'")
    letters_filter = paste0("AND ",detection_string," IN (",alphabets_string,")")
    
  } else{letters_filter = ""}
  
  
  
  query <- paste0("SELECT all_dxcodes.PatientDurableKey, all_dxcodes.Value as ICD10_Value,
  all_dxcodes.Year, all_dxcodes.YearMonth, all_dxcodes.Chronic,
  SUBSTRING(all_dxcodes.Value,1,3) AS Value_Grouper,
  SUBSTRING(all_dxcodes.Value,1,1) AS Value_Grouper2
  
  FROM(SELECT DISTINCT plf.PatientDurableKey, dtd.Value,dd.Year, dd.YearMonth, plf.Chronic
        FROM dbo.DiagnosisEventFact plf
	        INNER JOIN ",project_string," cohort 
	          ON plf.PatientDurableKey = cohort.PatientDurableKey
	        INNER JOIN dbo.DateDim dd
	         ON plf.StartDateKey = dd.DateKey
	        INNER JOIN dbo.DiagnosisTerminologyDim dtd
	         ON plf.DiagnosisKey = dtd.DiagnosisKey
        WHERE (plf.StartDateKey > ",filter_year,"0000) AND (plf.StartDateKey < ",filter_year+1,"0000) AND 
            (plf.StartDateKey >= (cohort.",IndexDateKey_Var,"-",LookBackInterval*10000,")) AND (plf.StartDateKey < (cohort.",
                  IndexDateKey_Var,"+",FollowUpInterval*10000,"))
	      AND dtd.Type = 'ICD-10-CM' ",letters_filter," 
	      ) all_dxcodes
  ")
  
  dbGetQuery(connection_Cosmos,query) %>% 
    return(.)
  
}

# For each filter_year == year,
# Check if a diagnosis falls within the EventDateKey's [LookBackInterval, FollowUpInterval] range
# A PatientDurableKey may have more than one observation in the larger parquet file because the range falls in two calendar years
# Use distinct(PatientDurableKey, ICD10_Value) on the parquet file or group_by(PatientDurableKey) to check if either of the years have the code of interest
# Cons: If the range is > 1 year, you cannot conclusively tell which year the code is detected RELATIVE to the index date.
# Example, IndexDateKey_Var's [0,2] range starts in June 2020. Year 1 will contain 2020 and 2021. Year 2 will contain 2021 and 2022.
# However, the dataset will collapse all codes from 2021 together
problemlist_start_Year_index <- function(connection_Cosmos,project_string,IndexDateKey_Var="EventDateKey",
                      LookBackInterval = 2,FollowUpInterval = 0,filter_year = numeric(),
                      by_letters = FALSE,alphabets=NULL){
  
  # See aap_t2dm/atdat06_ for example of when by_letters are used
  if(by_letters){
    
    alphabets_string = alphabets %>%  as.character() %>% paste0(.,collapse="','") %>% paste0("'",.,"'")
    letters_filter = paste0("AND SUBSTRING(dtd.Value,1,1) IN (",alphabets_string,")")
    
  } else{letters_filter = ""}
  
  
  
  query <- paste0("SELECT all_dxcodes.PatientDurableKey, all_dxcodes.Value as ICD10_Value, all_dxcodes.Year, all_dxcodes.Chronic,
  SUBSTRING(all_dxcodes.Value,1,3) AS Value_Grouper,
  SUBSTRING(all_dxcodes.Value,1,1) AS Value_Grouper2
  
  FROM(SELECT DISTINCT plf.PatientDurableKey, dtd.Value,dd.Year, plf.Chronic
        FROM dbo.DiagnosisEventFact plf
	        INNER JOIN ",project_string," cohort 
	          ON plf.PatientDurableKey = cohort.PatientDurableKey
	        INNER JOIN dbo.DateDim dd
	         ON plf.StartDateKey = dd.DateKey
	        INNER JOIN dbo.DiagnosisTerminologyDim dtd
	         ON plf.DiagnosisKey = dtd.DiagnosisKey
        WHERE (plf.StartDateKey > ",filter_year,"0000) AND (plf.StartDateKey < ",filter_year+1,"0000) AND 
            (plf.StartDateKey >= (cohort.",IndexDateKey_Var,"-",LookBackInterval*10000,")) AND (plf.StartDateKey < (cohort.",
                  IndexDateKey_Var,"+",FollowUpInterval*10000,"))
	      AND dtd.Type = 'ICD-10-CM' ",letters_filter," 
	      ) all_dxcodes
  ")
  
  dbGetQuery(connection_Cosmos,query) %>% 
    return(.)
  
}