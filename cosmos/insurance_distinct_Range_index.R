insurance_distinct_Range_index <- function(connection_Cosmos,project_string,IndexDateKey_Var=character(),
                      LowerInterval = 2,UpperInterval = 1,filter_year = numeric(),lower_geq = TRUE,upper_leq=FALSE){
  
  sign1 = case_when(LowerInterval <= 0 ~ " - ",
					TRUE ~ "+")
					
  sign2 = case_when(UpperInterval <= 0 ~ " - ",
					TRUE ~ "+")
  
  lower_gt_logic = case_when(lower_geq ~ ">=",
                             TRUE ~ ">")
  
  upper_lt_logic = case_when(upper_leq ~ "<=",
                             TRUE ~ "<")

  
  sql_query <- paste0("
  SELECT all_ecmfs.PatientDurableKey as PatientDurableKey,
  all_ecmfs.CoverageFinancialClass as CoverageFinancialClass,
  all_ecmfs.YearMonth as YearMonth, all_ecmfs.Year
  
  FROM(
      SELECT  DISTINCT ecmf.PatientDurableKey, ecmf.CoverageFinancialClass, DateDim.YearMonth, DateDim.Year
      FROM EncounterCoverageMappingFact ecmf
	    INNER JOIN ",project_string," cohort 
	          ON ecmf.PatientDurableKey = cohort.PatientDurableKey
	    INNER JOIN dbo.DateDim
	      ON ecmf.EncounterStartDateKey = DateDim.DateKey
      WHERE (ecmf.EncounterStartDateKey > ",filter_year,"0000) AND (ecmf.EncounterStartDateKey < ",filter_year+1,"0000) AND
            (ecmf.EncounterStartDateKey ",lower_gt_logic," (cohort.",IndexDateKey_Var,sign1,abs(LowerInterval)*10000,")) AND 
                      (ecmf.EncounterStartDateKey ",upper_lt_logic," (cohort.",
                      IndexDateKey_Var,sign2,abs(UpperInterval)*10000,"))
      ) all_ecmfs
  ")
  
  
  dbGetQuery(connection_Cosmos,sql_query) %>% 
    return(.)
  
}