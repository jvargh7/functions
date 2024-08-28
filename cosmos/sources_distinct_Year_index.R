sources_distinct_YearMonth_index <- function(connection_Cosmos,project_string,IndexDateKey_Var=character(),
                                          LookBackInterval = 0,FollowUpInterval = 1,filter_year = numeric()){
  
  
  query <- paste0("SELECT all_encs.PatientDurableKey, all_encs.YearMonth, all_encs.Year, all_encs.SourceKey, 
  all_encs.AttendingProviderDurableKey,
  all_encs.PrimarySpecialty, all_encs.SecondSpecialty
  
  FROM(SELECT DISTINCT ef.PatientDurableKey, dd.YearMonth, dd.Year, esb.SourceKey, 
  ef.AttendingProviderDurableKey,
  prov.PrimarySpecialty, prov.SecondSpecialty
        FROM dbo.EncounterFact ef
	        INNER JOIN ",project_string," cohort 
	          ON ef.PatientDurableKey = cohort.PatientDurableKey
	        INNER JOIN dbo.DateDim dd
	         ON ef.DateKey = dd.DateKey
	        INNER JOIN dbo.EncounterSourceBridge esb
	         ON ef.SourceComboKey = esb.EncounterSourceComboKey
	        INNER JOIN dbo.ProviderDim prov
	         ON ef.AttendingProviderDurableKey = prov.DurableKey
        WHERE (ef.DateKey > ",filter_year,"0000) AND (ef.DateKey < ",filter_year+1,"0000) AND 
            (ef.IsOutpatientFaceToFaceVisit = 1 OR ef.IsHospitalAdmission = 1 OR ef.IsHospitalOutpatientVisit = 1) AND
            (ef.DateKey >= (cohort.",IndexDateKey_Var,"-",LookBackInterval*10000,")) AND (ef.DateKey < (cohort.",
                  IndexDateKey_Var,"+",FollowUpInterval*10000,"))) all_encs
  ")
  
  dbGetQuery(connection_Cosmos,query) %>% 
    return(.)
  
}