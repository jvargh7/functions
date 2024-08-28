# library(tidyverse)

list_loincs <- list(
  hba1c = c('4548-4', '41995-2', '55454-3', '71875-9', '549-2', '17856-6', '59261-6', '62388-4', '17855-8', '10839-9'),
  fpg = c('1558-6', '76629-5', '77145-1', '1556-0', '35184-1', '14771-0'),
  rpg = c('2345-7', '2339-0', '41653-7', '2340-8', '27353-2', '1547-9'),
  ldl = c("13457-7","18262-6","2089-1","11054-4"),
  hdl = c("18263-4","2085-9"),
  tgl = c("12951-0","2571-8"),
  alt = c("1742-6","1744-2"),
  ast = c("1920-8"),
  creatinine <- c("2160-0")
  
)

labs <- function(connection_Cosmos,year,project_string,type = "hba1c"){
  
  loinc_list_query = paste0("
  SELECT * 
  FROM dbo.LabComponentDim
  WHERE LoincCode IN (", paste0(unlist(list_loincs[type]),collapse="','") %>% paste0("'",.,"'"),")")
  
  lab_query <- paste0("SELECT 
    lcrf.PatientDurableKey as PatientDurableKey,
    lcrf.PrioritizedDateKey as DateOfEvent,
    lcrf.NumericValue as NumericValue,
    loinc_codes.LoincCode as LoincCode,
    dd.Year
  FROM dbo.LabComponentResultFact as lcrf
  INNER JOIN (
            SELECT DISTINCT(cohort.PatientDurableKey) as PatientDurableKey
            FROM ",project_string," cohort 
          ) distinct_cohort
	         ON lcrf.PatientDurableKey = distinct_cohort.PatientDurableKey
	INNER JOIN ( ",loinc_list_query,
  " ) loinc_codes
	 ON lcrf.LabComponentKey = loinc_codes.LabComponentKey
	INNER JOIN (SELECT * FROM dbo.PatientDim WHERE UseInCosmosAnalytics_X =1) as pd
	 ON lcrf.PatientDurableKey = pd.DurableKey
	INNER JOIN dbo.DateDim dd
	 ON lcrf.PrioritizedDateKey = dd.DateKey
	WHERE (lcrf.PrioritizedDateKey > ",year,"0000) AND (lcrf.PrioritizedDateKey < ",year+1,"0000)
	AND lcrf.IsBlankOrUnsuccessfulAttempt = 0")
  
  dbGetQuery(connection_Cosmos,lab_query) %>% 
    return(.)
  
}
