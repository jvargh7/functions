library(vctrs)
is_not_labelled <- function(x){
  case_when(class(x) == "haven_labelled" ~ FALSE,
            TRUE ~ TRUE)
}

bind_rows_labelled <- function(...){
  # ... have to be a list of data.frame or tibbles
  
  dfs <- list(...)

  
  # dfs_coerced <- bind_rows(dfs)
  dfs_coerced <- plyr::rbind.fill(dfs)

  # Find if any site has labelled vector
  any_labelled <- imap(names(dfs_coerced),
                             function(v,name){
                              sapply(dfs,
                                     function(d){
                                       if(!(v %in% names(d))) { return(NA)}
                                       else if(is.labelled(d %>% dplyr::select(v) %>% pull())) {
                                         return(TRUE)
                                       } 
                                       else{return(FALSE)}
                                          })
                             }) %>% 
    data.frame() %>% 
    rename_all(~names(dfs_coerced)) %>% 
    mutate_all(as.numeric)
  
  non_labelled_cols <- c("pin","site_pin","adwc",names(any_labelled)[colSums(any_labelled,na.rm=TRUE)==0])
  any_labelled_cols <- names(any_labelled)[!names(any_labelled) %in% non_labelled_cols]
  
  ## Bind all the non_labelled_cols                        
  dfs_non_labelled_bound <- plyr::rbind.fill(lapply(dfs,
                                function(d){
                                  d %>% 
                                    select(one_of(non_labelled_cols))
                                }))
  # dfs_non_labelled_bound <- bind_rows(lapply(dfs,
  #                               function(d){
  #                                 d %>% 
  #                                   select(one_of(non_labelled_cols))
  #                               }))

  
  ## Extract labels of any_labelled_cols
  labels_cols <- imap(any_labelled_cols,
                       function(v,name){
                         name = sapply(dfs,
                                function(d){
                                  if(!(v %in% names(d))) { return(NA)}
                                  else if(is.labelled(d %>% dplyr::select(v) %>% pull())) {
                                    return(
                                      
                                      attr(d %>% 
                                             dplyr::select(v) %>% 
                                             pull(),
                                           "labels") %>% attr(.,"names")) %>% list(.)} 
                                  else{return(NA)}
                                })
                       })
  
  ## Find columns with identical labels
  identical_labels_cols <- sapply(labels_cols,function(x) {
    y = discard(x,
                ~all(is.na(.x))); 
    all(sapply(y[-1] %>% paste0(.,sep=""),
               FUN=identical,y[1] %>% paste0(.,sep="")));})
  
   ## Get identical labels
  identical_labels = lapply(labels_cols[identical_labels_cols],
         function(x){
           y = discard(x,
                       ~all(is.na(.x)));
           return(y[1][[1]])
         })
  
  ## Retrieve levels from the identical label cols
  identical_levels <- imap(any_labelled_cols[identical_labels_cols],
                      function(v,name){
                        name = sapply(dfs,
                                      function(d){
                                        if(!(v %in% names(d))) { return(NA)}
                                        else if(is.labelled(d %>% dplyr::select(v) %>% pull())) {
                                          return(
                                            
                                            attr(d %>% 
                                                   dplyr::select(v) %>% 
                                                   pull(),
                                                 "labels") %>% list(.) )} 
                                        else{return(NA)}
                                      })
                      }) %>% 
    lapply(.,
           function(x){
             y = discard(x,
                         ~all(is.na(.x)));
             return(y[1][[1]])
           })

  ## Binding non-factor columns
  dfs_character_bound <- map_df(dfs,
                               function(d){
                                 d %>% 
                                   select(one_of(any_labelled_cols[!identical_labels_cols])) %>% 
                                   mutate_all(function(x) as.character(as_factor(x)))
                                   })
  
  ## Binding factor columns
  dfs_factor_bound <- lapply(dfs,
                                function(d){
                                  d %>% 
                                    select(one_of(any_labelled_cols[identical_labels_cols]))
                                }) %>% 
    plyr::rbind.fill(.)
  # dfs_factor_bound <- lapply(dfs,
  #                               function(d){
  #                                 d %>% 
  #                                   select(one_of(any_labelled_cols[identical_labels_cols]))
  #                               }) %>% 
  #   bind_rows(.)
  
  ## Imputing identical level columns
  identical_levels_imputed <- map(dfs_factor_bound,
                                  function(x){
                                    x_imp = unique(x)[order(unique(x))];
                                    x_imp = x_imp[!is.na(x_imp)];
                                    return(x_imp)
                                  })
  
  identical_labels_imputed <- map(1:length(identical_labels),
                                  function(x){
                                    labels_imp = rep(NA,length(identical_levels_imputed[[x]]));
                                    levels_imp = identical_levels_imputed[[x]];
                                    levels = identical_levels[[x]];
                                    labels_imp[which(levels_imp %in% levels,arr.ind = TRUE)] <- identical_labels[[x]]
                                    labels_imp[-which(levels_imp %in% levels,arr.ind = TRUE)] <- levels_imp[-which(levels_imp %in% levels,arr.ind = TRUE)]
                                    return(labels_imp)
                                  })
  
  
  ## Rebinding factor columns
  dfs_factor_bound2 <- map2_df(dfs_factor_bound,1:length(identical_labels),
                             function(x=.x,y=.y){
                               print(y)
                               x_labelled = factor(x,
                                                   levels=identical_levels_imputed[[y]],
                                                   labels=identical_labels_imputed[[y]])
                               return(x_labelled)
                             })
  
  print("Bound factors")
  dfs_bound <- bind_cols(dfs_non_labelled_bound,dfs_character_bound,dfs_factor_bound2)
  
  return(dfs_bound)
  
  
  
  
}

get_var_types <- function(...){
  # ... have to be a list of data.frame or tibbles
  
  dfs <- list(...)
  
  var_list <- imap_dfr(dfs,function(x,n){
    df = data.frame(df_name = paste0("df",n),
               var_name = names(x)
               );
    var_types = lapply(x,class) %>% unlist();
    df <- df %>% 
      mutate(var_types = var_types)
    
    
  })
  
  var_list <- var_list %>% 
    spread(key = df_name,value = var_types)
  
  return(var_list)
  
}
