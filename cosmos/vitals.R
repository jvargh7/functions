


vitals <- function(connection_Cosmos,year,project_string){
  
  sql_query <- paste0("
  SELECT vitals.PatientDurableKey as PatientDurableKey,
		vitals.DateKey as DateKey,
		vitals.BodyMassIndex as BodyMassIndex,
		vitals.SystolicBloodPressure as SystolicBloodPressure,
		vitals.DiastolicBloodPressure as DiastolicBloodPressure,
		dd.Year as Year
		FROM VitalsFact vitals
		INNER JOIN (
            SELECT DISTINCT(cohort.PatientDurableKey) as PatientDurableKey
            FROM ",project_string," cohort 
          ) distinct_cohort
	         ON vitals.PatientDurableKey = distinct_cohort.PatientDurableKey
	  INNER JOIN dbo.DateDim dd
	    ON vitals.DateKey = dd.DateKey
		WHERE (vitals.DateKey > ",year,"0000) AND (vitals.DateKey < ",year+1,"0000)"
  )
  
  dbGetQuery(connection_Cosmos,sql_query) %>% 
    return(.)
  
}

