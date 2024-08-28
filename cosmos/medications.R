
project_string = "PROJECTS.ProjectD5A613.dbo.all_adults_aaprx"

medications <- function(connection_Cosmos,year,project_string,where_string = NA_character_){
  
  if(!is.na(where_string)){
    selected_meds <- paste0("
    INNER JOIN (SELECT * 
    FROM dbo.MedicationDim
    ",where_string," ) md 
    ON mof.MedicationKey = md.MedicationKey")
    
    date_vars_all_rxcodes = "all_rxcodes.Year, all_rxcodes.YearMonth"
    date_vars_dd = "dd.Year, dd.YearMonth"
    
  }else{
    selected_meds = "INNER JOIN dbo.MedicationDim md
                    ON mof.MedicationKey = md.MedicationKey"
    date_vars_all_rxcodes = "all_rxcodes.Year"
    date_vars_dd = "dd.Year"
    
  }
  
  query <- paste0("SELECT all_rxcodes.PatientDurableKey, all_rxcodes.TherapeuticClass, all_rxcodes.PharmaceuticalClass, ",
                  date_vars_all_rxcodes, ", all_rxcodes.SimpleGenericName
  
  FROM(SELECT DISTINCT mof.PatientDurableKey, md.TherapeuticClass, md.PharmaceuticalClass, ",
                  date_vars_dd, ", md.SimpleGenericName
       FROM dbo.MedicationOrderFact mof
       INNER JOIN (
         SELECT DISTINCT(cohort.PatientDurableKey) as PatientDurableKey
         FROM ",project_string," cohort 
         ) distinct_cohort
        ON mof.PatientDurableKey = distinct_cohort.PatientDurableKey 
        INNER JOIN dbo.DateDim dd
        ON mof.StartDateKey = dd.DateKey ",
        selected_meds,
        " WHERE (mof.StartDateKey > ",year,"0000) AND (mof.StartDateKey < ",year+1,"0000)
       ) all_rxcodes
  ")
  
  dbGetQuery(connection_Cosmos,query) %>% 
    return(.)
  
}


# medications_specific <- function(connection_Cosmos,year,project_string,where_string){
  
  
  
 
  
# }

