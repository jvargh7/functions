encounter_count_Year_index <- function(connection_Cosmos,project_string,IndexDateKey_Var=character(),
                                          LookBackInterval = 1,FollowUpInterval = 0,filter_year = numeric()){
  
	# If an 'IndexDateKey_Var' encounters are spread across multiple calendar years based on the interval:
	# We will need to sum them up across all Years using dplyr::group_by() and dplyr::summarize()
  query <- paste0("
  
		SELECT MIN(ef.PatientDurableKey) AS PatientDurableKey, 
		MIN(dd.Year) AS Year, 
		MIN(cohort.",IndexDateKey_Var,") as ",IndexDateKey_Var,", 
		COUNT(*) AS n_encounters, 
        FROM dbo.EncounterFact ef
	        INNER JOIN ",project_string," cohort 
	          ON ef.PatientDurableKey = cohort.PatientDurableKey
	        INNER JOIN dbo.DateDim dd
	         ON ef.DateKey = dd.DateKey
        WHERE (ef.DateKey > ",filter_year,"0000) AND (ef.DateKey < ",filter_year+1,"0000) AND 
            (ef.IsOutpatientFaceToFaceVisit = 1 OR ef.IsHospitalAdmission = 1 OR ef.IsHospitalOutpatientVisit = 1) AND
            (ef.DateKey >= (cohort.",IndexDateKey_Var,"-",LookBackInterval*10000,")) AND (ef.DateKey < (cohort.",
                  IndexDateKey_Var,"+",FollowUpInterval*10000,"))
		GROUP BY ef.PatientDurableKey, dd.Year, cohort.",IndexDateKey_Var,")		  
  ")

  
  dbGetQuery(connection_Cosmos,query) %>% 
    return(.)
  
}