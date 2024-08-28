insurance <- function(connection_Cosmos,year,project_string){
# Extract all distinct insurance values by YearMonth for calendar year
# This is independent of an 'IndexDateKey_Var'
 
sql_query <- paste0("
  SELECT all_ecmfs.PatientDurableKey as PatientDurableKey,
  all_ecmfs.CoverageFinancialClass as CoverageFinancialClass,
  all_ecmfs.YearMonth as YearMonth, all_ecmfs.Year
  
  FROM(
      SELECT  DISTINCT ecmf.PatientDurableKey, ecmf.CoverageFinancialClass, DateDim.YearMonth, DateDim.Year
      FROM EncounterCoverageMappingFact ecmf
      INNER JOIN (
            SELECT DISTINCT(cohort.PatientDurableKey) as PatientDurableKey
            FROM ",project_string," cohort 
          ) distinct_cohort
	         ON ecmf.PatientDurableKey = distinct_cohort.PatientDurableKey
	    INNER JOIN dbo.DateDim
	      ON ecmf.EncounterStartDateKey = DateDim.DateKey
      WHERE (ecmf.EncounterStartDateKey > ",year,"0000) AND (ecmf.EncounterStartDateKey < ",year+1,"0000)
      ) all_ecmfs
  ")
  
  
  dbGetQuery(connection_Cosmos,sql_query) %>% 
    return(.)
  
}