



encounter_distinct_Year_index <- function(connection_Cosmos,project_string,IndexDateKey_Var=character(),
                                          LookBackInterval = 2,FollowUpInterval = 0,filter_year = numeric()){
  

  query <- paste0("SELECT all_encs.PatientDurableKey, all_encs.Year, all_encs.Count
  
  FROM(SELECT DISTINCT ef.PatientDurableKey, dd.Year, ef.Count
        FROM dbo.EncounterFact ef
	        INNER JOIN ",project_string," cohort 
	          ON ef.PatientDurableKey = cohort.PatientDurableKey
	        INNER JOIN dbo.DateDim dd
	         ON ef.DateKey = dd.DateKey
        WHERE (ef.DateKey > ",filter_year,"0000) AND (ef.DateKey < ",filter_year+1,"0000) AND 
            (ef.IsOutpatientFaceToFaceVisit = 1 OR ef.IsHospitalAdmission = 1 OR ef.IsHospitalOutpatientVisit = 1) AND
            (ef.DateKey >= (cohort.",IndexDateKey_Var,"-",LookBackInterval*10000,")) AND (ef.DateKey < (cohort.",
                  IndexDateKey_Var,"+",FollowUpInterval*10000,"))) all_encs
  ")
  
  dbGetQuery(connection_Cosmos,query) %>% 
    return(.)
  
}

