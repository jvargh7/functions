variable_labels <- function(df,type="dta"){
  if(type=="dta"){
    library(labelled)
    # library(dataMeta)
    existing_labels <- look_for(df) %>%
      pull(label)
    
    # Override default to print levels of a continuous variable 
    vars_display_levels <- c()
    
    # Create single vector of variable type indicator
    var_types <- tibble(var = names(df), class = sapply(df, class),
                        type = case_when(class %in% c('character', 'factor') ~ 1,
                                         var %in% vars_display_levels ~ 1,
                                         TRUE ~ 0)) %>%
      pull(type)
    
    # Create linker dataframe
    link <- dataMeta::build_linker(df, existing_labels, var_types) %>% 
      mutate(class = sapply(df,class))
  }
  
  if(type %in% c("sas7bdat","RDS","sav","dta2")){
    
    # df = read_sas(paste0("C:/Cloud/Box Sync/INCAP SES Harmonization/RSPH Research/Raw and old data/human capital/data_Apr06/SAS/ses/ses_all_apr06.sas7bdat"))
    var_name = colnames(df)
    # var_desc = rep(NA,times=length(var_name))
    # library(dataMeta)
    
    var_desc = map(df,function(x) {
      if(is.null(attr(x,"label",exact = TRUE))) {return(NA)};
      if(!is.null(attr(x,"label",exact = TRUE))) return(attr(x,"label"))
    }) %>% 
      unlist()
    # print(head(var_desc))
    # Override default to print levels of a continuous variable 
    # vars_display_levels <- c()
    
    var_types <- tibble(var = names(df), 
                        class = sapply(df, class),
                        type = case_when(class %in% c('character', 'factor') ~ 1,
                                         # var %in% vars_display_levels ~ 1,
                                         TRUE ~ 0)) %>%
      pull(type)
    
    class = ifelse(var_types==1,"character","numeric")
    
    link = data.frame(var_name,
                      var_desc,
                      var_type = var_types,
                      class) 
    
    rownames(link) <- NULL
    
  }
  
  
  if(type == "csv"){
    var_name = names(df)
    var_desc = rep(NA,times=length(var_name))
    
    var_type = sapply(df,function(x) ifelse(length(unique(x)) < 9,1,0))
    
    class = ifelse(var_type==1,"character","numeric")
    
    link = data.frame(var_name,
                      var_desc,
                      var_type,
                      class) 
    
    rownames(link) <- NULL
    
  }
  
  
  
  return(link)
}

value_labels <- function(df, type ="dta"){
  
  if(type=="dta"){
    library(labelled) 
    
    value_label_df = imap_dfr(df,function(x,name) {
      if(length(val_labels(x)) == 0){
        data.frame(var_name = name,
                   var_labels = NA,
                   var_levels=NA)};
      if(length(val_labels(x))>0){
        data.frame(var_name = name,
                   var_labels = names(val_labels(x)),
                   var_levels = val_labels(x) %>% unlist())}})
  }
  
  
}



dictionary_file <- function(df,type = "dta",name = NULL){
  
  name = ifelse(is.null(name),deparse(substitute(df)),name)
  
  print(str_replace_all(tempdir() ,"\\\\","/"))
  dictionary_df <- tryCatch({left_join(variable_labels(df,type=type),
                                       value_labels(df,type=type),
                                       by="var_name")},
                            error = function(e){
                              variable_labels(df,type=type)
                            }) %>% 
    
    mutate(class = sapply(.[,"class"],function(x) paste0(x,collapse=";")))
  
  write.csv(dictionary_df, file = paste0(tempdir(), "/dictionary_",
                                         name,".csv"))
  
}
