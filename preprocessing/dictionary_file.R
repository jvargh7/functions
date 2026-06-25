variable_labels <- function(df,type="dta"){
  # if(type=="dta"){
  #   library(labelled)
  #   # library(dataMeta)
  #   existing_labels <- look_for(df) %>%
  #     pull(label)
    
  #   # Override default to print levels of a continuous variable 
  #   vars_display_levels <- c()
    
  #   # Create single vector of variable type indicator
  #   # var_types <- tibble(var = names(df), class = sapply(df, class),
  #   #                     type = case_when(class %in% c('character', 'factor') ~ 1,
  #   #                                      var %in% vars_display_levels ~ 1,
  #   #                                      TRUE ~ 0)) %>%
  #   #   pull(type)
    
  #   var_types <- tibble(var = names(df), 
  #                       class = map(df, function(x) paste0(class(x),collapse="-")) %>% as.character(),
  #                       type = case_when(str_detect(class,"(character|factor)") ~ 1,
  #                                        var %in% vars_display_levels ~ 1,
  #                                        TRUE ~ 0)) %>%
  #     pull(type)
    
  #   existing_labels[is.na(existing_labels)] <- ""
    
    
  #   library(dataMeta)
  #   # Create linker dataframe
  #   link <- build_linker(df, existing_labels, var_types) %>% 
  #     mutate(class = map(df, function(x) paste0(class(x),collapse="-")) %>% as.character())
  # }
  
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
                        class = map(df, function(x) paste0(class(x),collapse="-")) %>% as.character(),
                        type = case_when(str_detect(class,"(character|factor)") ~ 1,
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
  
  if(type %in% c("dta","dta2")){
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

one_line_descriptives <- function(df_input, dictionary_df = NULL) {
  
  classify_var <- function(x) {
    cls <- class(x)
    # haven_labelled (and haven_labelled_spss) vectors carry an underlying
    # typeof() of "double"/"integer"/"character" plus value label attributes.
    if (inherits(x, "haven_labelled")) {
      lbls <- attr(x, "labels", exact = TRUE)
      if (typeof(x) == "character" || length(lbls) > 0) {
        return("categorical")
      } else {
        return("continuous")
      }
    }
    if (any(cls %in% c("factor", "character", "logical"))) {
      "categorical"
    } else if (any(cls %in% c("numeric", "integer", "double"))) {
      "continuous"
    } else {
      "other"
    }
  }
  
  summarize_continuous <- function(x) {
    x <- as.numeric(x)   # strips haven_labelled / integer64 etc.
    q <- quantile(x, probs = c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE)
    paste0(
      "min=", round(q[[1]], 3), "; ",
      "q25=", round(q[[2]], 3), "; ",
      "median=", round(q[[3]], 3), "; ",
      "q75=", round(q[[4]], 3), "; ",
      "max=", round(q[[5]], 3)
    )
  }
  
  summarize_categorical <- function(x) {
    # Convert haven_labelled to factor so value labels appear in the summary
    # rather than raw numeric codes.
    if (inherits(x, "haven_labelled")) {
      x <- haven::as_factor(x)
    }
    tbl <- table(x, useNA = "no")
    n   <- sum(tbl)
    if (n == 0) return(NA_character_)
    pct <- round(100 * tbl / n, 1)
    paste(names(pct), paste0(pct, "%"), sep = "=", collapse = "; ")
  }
  
  descriptives_out <- purrr::imap_dfr(df_input, function(col, name) {
    vtype    <- classify_var(col)
    vsummary <- switch(vtype,
      "continuous"  = tryCatch(summarize_continuous(col),  error = function(e) NA_character_),
      "categorical" = tryCatch(summarize_categorical(col), error = function(e) NA_character_),
      "other"       = "not applicable"
    )
    tibble::tibble(var_name = name, var_type_desc = vtype, var_summary = vsummary)
  })

  # print(head(descriptives_out))

  return(descriptives_out)
}



dictionary_file <- function(df,type = "dta",name = NULL,return_dictionary=FALSE){
  
  name = ifelse(is.null(name),deparse(substitute(df)),name)
  
  print(str_replace_all(tempdir() ,"\\\\","/"))
  dictionary_df <- tryCatch({left_join(variable_labels(df,type=type),
                                       value_labels(df,type=type),
                                       by="var_name")},
                            error = function(e){
                              variable_labels(df,type=type)
                            }) %>% 
    
    mutate(class = sapply(.[,"class"],function(x) paste0(x,collapse=";")))
  
 
  
  # names(dictionary_df)
  desc_df = one_line_descriptives(df)
  # print(head(desc_df))
  if (!is.null(dictionary_df)) {
    if(!is.null(desc_df)){
      dictionary_df_desc <- dplyr::left_join(dictionary_df, desc_df, by = "var_name")
      print(head(dictionary_df_desc))
      
      write.csv(dictionary_df_desc, file = paste0(tempdir(), "/dictionary_df_desc_",
                                                  name,".csv"))
    }else{
      write.csv(dictionary_df, file = paste0(tempdir(), "/dictionary_df_",
                                             name,".csv"))
    }
    
  }

  if(return_dictionary){
    return(dictionary_df_desc)
  }
  
  
}


