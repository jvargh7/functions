# For each filter_year == year,
# Check if a diagnosis falls within the EventDateKey's [LookBackInterval, FollowUpInterval] range
# A PatientDurableKey may have more than one observation in the larger parquet file because the range falls over several calendar years
# Use distinct(PatientDurableKey, ICD10_Value) on the parquet file or group_by(PatientDurableKey) to check if any of the years have the code of interest


procedures_distinct_YearMonth_index <- function(connection_Cosmos,project_string,IndexDateKey_Var="EventDateKey",
                                               LookBackInterval = 2,FollowUpInterval = 0,filter_year = numeric(),
                                               by_codes = FALSE,cpt_codes=NULL,
                                               detection_string = "procd.CptCode"){
  
  if(by_codes){
    
    procedures_string = cpt_codes %>%  as.character() %>% paste0(.,collapse="','") %>% paste0("'",.,"'")
    codes_filter = paste0("AND ",detection_string," IN (",procedures_string,")")
    
  } else{codes_filter = ""}
  
  
  
  query <- paste0("SELECT all_proccodes.PatientDurableKey, all_proccodes.CptCode, 
  all_proccodes.Year, all_proccodes.YearMonth,
  all_proccodes.Category, all_proccodes.Name, all_proccodes.ShortName
  
  FROM(SELECT DISTINCT pef.PatientDurableKey, procd.CptCode, dd.Year, dd.YearMonth, procd.Category, procd.Name, procd.ShortName
        FROM dbo.ProcedureEventFact pef
	        INNER JOIN ",project_string," cohort 
	          ON pef.PatientDurableKey = cohort.PatientDurableKey
	        INNER JOIN dbo.DateDim dd
	         ON pef.ProcedureStartDateKey = dd.DateKey
	        INNER JOIN dbo.ProcedureDim procd
	         ON pef.ProcedureDurableKey = procd.DurableKey
        WHERE (pef.ProcedureStartDateKey > ",filter_year,"0000) AND (pef.ProcedureStartDateKey < ",filter_year+1,"0000) AND 
            (pef.ProcedureStartDateKey >= (cohort.",IndexDateKey_Var,"-",LookBackInterval*10000,")) AND (pef.ProcedureStartDateKey < (cohort.",
                  IndexDateKey_Var,"+",FollowUpInterval*10000,"))
	      AND procd.CodeSet = N'CPT(R)' ",codes_filter," 
	      ) all_proccodes
  ")
  
  dbGetQuery(connection_Cosmos,query) %>% 
    return(.)
  
}

procedures_distinct_Year_index <- function(connection_Cosmos,project_string,IndexDateKey_Var="EventDateKey",
                                                LookBackInterval = 2,FollowUpInterval = 0,filter_year = numeric(),
                                                by_codes = FALSE,cpt_codes=NULL,
                                                detection_string = "procd.CptCode"){
  
  if(by_codes){
    
    procedures_string = cpt_codes %>%  as.character() %>% paste0(.,collapse="','") %>% paste0("'",.,"'")
    codes_filter = paste0("AND ",detection_string," IN (",procedures_string,")")
    
  } else{codes_filter = ""}
  
  
  
  query <- paste0("SELECT all_proccodes.PatientDurableKey, all_proccodes.CptCode, 
  all_proccodes.Year, 
  all_proccodes.Category, all_proccodes.Name, all_proccodes.ShortName
  
  FROM(SELECT DISTINCT pef.PatientDurableKey, procd.CptCode, dd.Year, procd.Category, procd.Name, procd.ShortName
        FROM dbo.ProcedureEventFact pef
	        INNER JOIN ",project_string," cohort 
	          ON pef.PatientDurableKey = cohort.PatientDurableKey
	        INNER JOIN dbo.DateDim dd
	         ON pef.ProcedureStartDateKey = dd.DateKey
	        INNER JOIN dbo.ProcedureDim procd
	         ON pef.ProcedureDurableKey = procd.DurableKey
        WHERE (pef.ProcedureStartDateKey > ",filter_year,"0000) AND (pef.ProcedureStartDateKey < ",filter_year+1,"0000) AND 
            (pef.ProcedureStartDateKey >= (cohort.",IndexDateKey_Var,"-",LookBackInterval*10000,")) AND (pef.ProcedureStartDateKey < (cohort.",
                  IndexDateKey_Var,"+",FollowUpInterval*10000,"))
	      AND procd.CodeSet = N'CPT(R)' ",codes_filter," 
	      ) all_proccodes
  ")
  
  dbGetQuery(connection_Cosmos,query) %>% 
    return(.)
  
}