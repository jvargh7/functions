
# project_string example: "PROJECTS.ProjectD0C076.dbo.diabetic_retinopathy"
# Please make sure that the cohort table has retained the same variable name (PatientDurableKey) for the patient ID
# pd.SviHousingTypeTransportationPctlRankingByZip2020_X as SviHousingTypeTransportationPctlRankingByZip2020_X,
# pd.SviSocioeconomicPctlRankingByZip2020_X as SviSocioeconomicPctlRankingByZip2020_X,

demographics <- function(connection_Cosmos,project_string){
  
  sql_demographics <- 
    paste0("SELECT cohort.PatientDurableKey as PatientDurableKey,
  pd.BirthDate as BirthDate,
  pd.DeathDate as DeathDate,
  pd.Status as Status,
  pd.Country as Country,
  pd.SourceCountry_X as SourceCountry_X,
  pd.ValidatedCountry_X as ValidatedCountry_X,
  pd.ValidatedStateOrProvince_X as ValidatedStateOrProvince_X,
  pd.PrimaryRUCA_X as PrimaryRUCA_X,
  pd.Ethnicity as Ethnicity,
  pd.MultiRacial as MultiRacial,
  pd.FirstRace as FirstRace,
  pd.SecondRace as SecondRace,
  pd.ThirdRace as ThirdRace,
  pd.Sex as Sex,
  pd.GenderIdentity as GenderIdentity,
  pd.SexAssignedAtBirth as SexAssignedAtBirth,
  pd.MaritalStatus as MaritalStatus,
  pd.SviHouseholdCharacteristicsPctlRankByZip2020_X as SviHouseholdCharacteristicsPctlRankByZip2020_X,
  pd.SviRacialEthnicMinorityStatusPctlRankByZip2020_X as SviRacialEthnicMinorityStatusPctlRankByZip2020_X,
  pd.SviOverallPctlRankByZip2020_X as SviOverallPctlRankByZip2020_X
  

  FROM (SELECT DISTINCT(PatientDurableKey) as PatientDurableKey
           
  FROM ",project_string,") cohort
  INNER JOIN dbo.PatientDim pd
    ON cohort.PatientDurableKey = pd.DurableKey
")
  
  dbGetQuery(connection_Cosmos, sql_demographics) %>% 
    return(.)
  
}