library(pdftools)
library(tidyverse)
pdf_compiler <- function(folder_path){
  
  file_list = list.files(folder_path,full.names = TRUE)
  
  folder_name = stringr::str_extract(folder_path,"[A-Za-z0-9_\\-]+$")
  
  file_list = file_list[!file_list %in% paste0(folder_path,folder_name,".pdf")]
  
  temp_path = tempdir()
  print(temp_path)
  purrr::map(file_list,
      function(f_l){
        file_name = stringr::str_replace(f_l,pattern=folder_path,replacement = "") %>% stringr::str_replace(.,"/","")
        
        f_length = pdf_length(f_l)
        
        if(f_length > 1){
          pdf_subset(input = f_l,
                     pages=1:2,
                     output = paste0(str_replace_all(temp_path,"\\\\","/"),"/",file_name))
          
        } else{file.copy(f_l,paste0(str_replace_all(temp_path,"\\\\","/"),"/",file_name))
        }
        
        
        
      })
  
  split_files_list = list.files(temp_path,full.names = TRUE)
  split_files_list = split_files_list[str_detect(split_files_list,"\\.pdf")]
  
  output_path = paste0(folder_path,"/Hamming Friday_",folder_name,".pdf")
  
  pdf_combine(split_files_list,output = output_path)
  
}
