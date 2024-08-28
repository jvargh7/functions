vitals_YearMonth_index <- function(connection_Cosmos,project_string,IndexDateKey_Var="EventDateKey",
                                   LookBackInterval = 2,FollowUpInterval = 0,filter_year = numeric()){
  
  sql_query <- paste0("
  SELECT MIN(vitals.PatientDurableKey) as PatientDurableKey,
		AVG(vitals.BodyMassIndex) as BodyMassIndex,
		AVG(vitals.SystolicBloodPressure) as SystolicBloodPressure,
		AVG(vitals.DiastolicBloodPressure) as DiastolicBloodPressure,
		MIN(dd.YearMonth) as YearMonth,
		MIN(dd.Year) as Year
		FROM VitalsFact vitals
		INNER JOIN ",project_string," cohort 
	  ON vitals.PatientDurableKey = cohort.PatientDurableKey
	  INNER JOIN dbo.DateDim dd
	    ON vitals.DateKey = dd.DateKey
		WHERE (vitals.DateKey > ",filter_year,"0000) AND (vitals.DateKey < ",filter_year+1,"0000) AND
          (vitals.DateKey >= (cohort.",IndexDateKey_Var,"-",LookBackInterval*10000,")) AND (vitals.DateKey < (cohort.",
                      IndexDateKey_Var,"+",FollowUpInterval*10000,"))
    GROUP BY vitals.PatientDurableKey,dd.YearMonth"
  )
  
  dbGetQuery(connection_Cosmos,sql_query) %>% 
    return(.)
  
}


vitals_index <- function(connection_Cosmos,project_string,IndexDateKey_Var="EventDateKey",LookBackInterval = 2,FollowUpInterval = 0,filter_year = numeric()){
  
  sql_query <- paste0("
  SELECT vitals.PatientDurableKey as PatientDurableKey,
		vitals.DateKey as DateKey,
		vitals.BodyMassIndex as BodyMassIndex,
		vitals.SystolicBloodPressure as SystolicBloodPressure,
		vitals.DiastolicBloodPressure as DiastolicBloodPressure,
		dd.Year as Year
		FROM VitalsFact vitals
		INNER JOIN ",project_string," cohort 
	  ON vitals.PatientDurableKey = cohort.PatientDurableKey
	  INNER JOIN dbo.DateDim dd
	    ON vitals.DateKey = dd.DateKey
		WHERE (vitals.DateKey > ",filter_year,"0000) AND (vitals.DateKey < ",filter_year+1,"0000) AND
          (vitals.DateKey >= (cohort.",IndexDateKey_Var,"-",LookBackInterval*10000,")) AND (vitals.DateKey < (cohort.",
                      IndexDateKey_Var,"+",FollowUpInterval*10000,"))"
  )
  
  dbGetQuery(connection_Cosmos,sql_query) %>% 
    return(.)
  
}
