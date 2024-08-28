
# For each filter_year == year,
# Check if a medication falls within the EventDateKey's [LookBackInterval, FollowUpInterval] range
# A PatientDurableKey may have more than one observation in the larger parquet file because the range falls in two calendar years
# Use distinct(PatientDurableKey, SimpleGenericName) on the parquet file or group_by(PatientDurableKey) to check if either of the years have the rx of interest
# Cons: If the range is > 1 year, you cannot conclusively tell which year the rx is detected RELATIVE to the index date.
# Example, IndexDateKey_Var's [0,2] range starts in June 2020. Year 1 will contain 2020 and 2021. Year 2 will contain 2021 and 2022.
# However, the dataset will collapse all codes from 2021 together
medications_distinct_Year_index <- function(connection_Cosmos,project_string,IndexDateKey_Var="EventDateKey",
                                            LookBackInterval = 2,FollowUpInterval = 0,filter_year = numeric()){
  
  query <- paste0("SELECT all_rxcodes.PatientDurableKey, 
  all_rxcodes.TherapeuticClass, all_rxcodes.PharmaceuticalClass, all_rxcodes.PharmaceuticalSubClass,  all_rxcodes.Year,
  all_rxcodes.SimpleGenericName
  
  FROM(SELECT DISTINCT mof.PatientDurableKey, md.TherapeuticClass, md.PharmaceuticalClass, md.PharmaceuticalSubClass, dd.Year, md.SimpleGenericName
       FROM dbo.MedicationOrderFact mof
       INNER JOIN ",project_string," cohort 
	      ON mof.PatientDurableKey = cohort.PatientDurableKey
       INNER JOIN dbo.DateDim dd
        ON mof.StartDateKey = dd.DateKey
       INNER JOIN dbo.MedicationDim md
        ON mof.MedicationKey = md.MedicationKey
       WHERE mof.EndDateKey > 0 AND (mof.StartDateKey > ",filter_year,"0000) AND (mof.StartDateKey < ",filter_year+1,"0000) AND 
            (mof.StartDateKey >= (cohort.",IndexDateKey_Var,"-",LookBackInterval*10000,")) AND (mof.StartDateKey < (cohort.",
                  IndexDateKey_Var,"+",FollowUpInterval*10000,"))
       ) all_rxcodes
  ")
  
  dbGetQuery(connection_Cosmos,query) %>% 
    return(.)
  
}


