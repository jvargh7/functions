encounter_count_Range_index <- function(connection_Cosmos,project_string,IndexDateKey_Var=character(),
                                          LowerInterval = 2,UpperInterval = 1,filter_year = numeric()){
  
 
  sign1 = case_when(LowerInterval <= 0 ~ "-",
					TRUE ~ "+")
					
  sign2 = case_when(UpperInterval <= 0 ~ "-",
					TRUE ~ "+")

 
  query <- paste0("SELECT all_encs.PatientDurableKey, all_encs.",IndexDateKey_Var,", all_encs.Year, all_encs.N_enc 
  
  FROM(SELECT COUNT_BIG(*) as N_enc, MIN(ef.PatientDurableKey) as PatientDurableKey, MIN(cohort.",IndexDateKey_Var,") as ",IndexDateKey_Var,",
  MIN(dd.Year) as Year
        FROM dbo.EncounterFact ef
	        INNER JOIN ",project_string," cohort 
	          ON ef.PatientDurableKey = cohort.PatientDurableKey
	        INNER JOIN dbo.DateDim dd
	         ON ef.DateKey = dd.DateKey
        WHERE (ef.DateKey > ",filter_year,"0000) AND (ef.DateKey < ",filter_year+1,"0000) AND 
            (ef.IsOutpatientFaceToFaceVisit = 1 OR ef.IsHospitalAdmission = 1 OR ef.IsHospitalOutpatientVisit = 1) AND
            (ef.DateKey >= (cohort.",IndexDateKey_Var,sign1,abs(LowerInterval)*10000,")) AND (ef.DateKey < (cohort.",
                  IndexDateKey_Var,sign2,abs(UpperInterval)*10000,"))
		GROUP BY ef.PatientDurableKey, cohort.",IndexDateKey_Var,") all_encs
  ")
  
  dbGetQuery(connection_Cosmos,query) %>% 
    return(.)
  
}
