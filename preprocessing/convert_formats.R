
file_path <- "C:/Cloud/OneDrive - Emory University/Proposals/Trajectories of Phenotypes/working/jhs/Data/Analysis_Data"
file_name <- "analysis1.sas7bdat"
require(tidyverse)


read_formats <- function(file_path,file_name){
  
  if(str_detect(file_name,pattern="sas7bdat")){
    
    df <- haven::read_sas(paste0(file_path,"/",file_name))
    
  }
  
  if(str_detect(file_name,pattern="dta")){
    
    df <- haven::read_dta(paste0(file_path,"/",file_name))
    
  }
  
  if(str_detect(file_name,pattern="csv")){
    
    df <- readr::read_csv(paste0(file_path,"/",file_name))
    
  }
  
  if(str_detect(file_name,pattern="xls")){
    
    df <- readxl::read_excel(paste0(file_path,"/",file_name))
    
  }
  
  if(str_detect(file_name,pattern="RDS")){
    
    df <- readRDS(paste0(file_path,"/",file_name))
    
  }
  
  return(df)
  
}

write_formats <- function(df,orig_file_name,dest_type,dest_path){
  
  dest_name <- str_replace(orig_file_name,"\\..*","") %>% paste0(.,".",dest_type)
  
  if(dest_type=="sas7bdat"){
    
    df <- haven::write_sas(df,paste0(dest_path,"/",dest_name))
    
  }
  
  if(dest_type=="dta"){
    
    df <- haven::write_dta(df,paste0(dest_path,"/",dest_name))
    
  }
  
  if(dest_type=="csv"){
    
    df <- readr::write_csv(df,paste0(dest_path,"/",dest_name))
    
  }
  
  if(dest_type %in% c("xls","xlsx")){
    
    df <- openxlsx::write.xlsx(df,paste0(dest_path,"/",dest_name))
    
  }
  
  if(dest_type=="RDS"){
    
    df <- saveRDS(df,paste0(dest_path,"/",dest_name))
    
  }
  
  return(df)
  
}


convert_formats <- function(file_path, file_name, dest_type = "csv",dest_path = character()){
  
  
  if(identical(dest_path,character(0))){
    
    dest_path <- file_path
    
  }
  
  if(!dir.exists(dest_path)){
    dir.create(dest_path)
  }
  
  read_formats(file_path,file_name) %>% 
    write_formats(.,file_name,dest_type,dest_path)
  
  
  
  
}