
medications_distinct_Range_index <- function(connection_Cosmos,project_string,IndexDateKey_Var="EventDateKey",
                                             LowerInterval = 2,UpperInterval = 1,filter_year = numeric(),end_date = FALSE){
  
  sign1 = case_when(LowerInterval <= 0 ~ " - ",
                    TRUE ~ "+")
  
  sign2 = case_when(UpperInterval <= 0 ~ " - ",
                    TRUE ~ "+")
  
  end_date_criterion = case_when(end_date ~ "AND mof.EndDateKey > 0",
                                 TRUE ~ "")
  
  
  query <- paste0("SELECT all_rxcodes.PatientDurableKey, 
  all_rxcodes.TherapeuticClass, all_rxcodes.PharmaceuticalClass, all_rxcodes.PharmaceuticalSubClass, all_rxcodes.Year,all_rxcodes.YearMonth,
  all_rxcodes.SimpleGenericName, all_rxcodes.Months
  
  FROM(SELECT DISTINCT mof.PatientDurableKey, md.TherapeuticClass, md.PharmaceuticalClass, md.PharmaceuticalSubClass, 
  dd.Year, dd.YearMonth, md.SimpleGenericName, dur.Months
       FROM dbo.MedicationOrderFact mof
       INNER JOIN ",project_string," cohort 
	      ON mof.PatientDurableKey = cohort.PatientDurableKey
       INNER JOIN dbo.DateDim dd
        ON mof.StartDateKey = dd.DateKey
       INNER JOIN dbo.DurationDim dur
        ON mof.DurationKey = dur.DurationKey
       INNER JOIN dbo.MedicationDim md
        ON mof.MedicationKey = md.MedicationKey
       WHERE (mof.StartDateKey > ",filter_year,"0000) AND (mof.StartDateKey < ",filter_year+1,"0000) AND 
            (mof.StartDateKey >= (cohort.",IndexDateKey_Var,sign1,abs(LowerInterval)*10000,")) AND (mof.StartDateKey < (cohort.",
                  IndexDateKey_Var,sign2,abs(UpperInterval)*10000,"))
                  ",end_date_criterion,"
       ) all_rxcodes
  ")
  
  dbGetQuery(connection_Cosmos,query) %>% 
    return(.)
  
}