

bibliography_api <- function(cited){
  
  doi_extract <- map_dfr(1:length(cited),
                         function(i){
                           c = cited[i];
                           doi = str_extract(c,"doi:.*") %>% str_replace(.,"doi:","");
                           n = str_extract(c,"^[0-9]+");
                           print(i);
                           output = data.frame(row = i,
                                               PMID = NA_real_,
                                               PMCID = NA_character_,
                                               DOI = doi);
                           
                           
                           if(!identical(doi,NA_character_)){
                             # https://www.ncbi.nlm.nih.gov/pmc/tools/id-converter-api/
                             output =  read_csv(paste0("https://www.ncbi.nlm.nih.gov/pmc/utils/idconv/v1.0/?ids=",
                                                       doi,"&format=csv")) %>% 
                               mutate(row = i)
                             
                             # To address reference 10.1111/dom.14063
                             if(nrow(output) > 1){
                               output <- output %>% 
                                 dplyr::filter(IsCurrent == 1)
                             }
                             
                             if(is.na(output$PMID)){
                               # https://www.flickr.com/photos/dullhunk/454160748
                               pmid = try({read_xml(paste0("https://eutils.ncbi.nlm.nih.gov/entrez/eutils/esearch.fcgi?db=PubMed&retmode=xml&term=",
                                                           doi))  %>% 
                                   xml_find_first("IdList") %>% 
                                   xml_find_all("Id") %>% 
                                   as_list() %>% 
                                   unlist()})
                               
                               if(length(pmid) == 1){
                                 output$PMID = as.numeric(pmid)
                               }
                               
                               
                             }
                             
                             
                           }
                           
                           return(output)
                           
                         })
  
  
  
  
  cited_updated <- map(1:length(cited),
                       function(i){
                         c = cited[i];
                         
                         doi = na.omit(doi_extract[i,]$DOI) %>% as.character()
                         pmid = na.omit(doi_extract[i,]$PMID)  %>% as.character()
                         pmcid = na.omit(doi_extract[i,]$PMCID)  %>% as.character()
                         
                         if(!identical(doi,character(0))){
                           original = paste0("doi:",doi)
                           
                           replacement = paste0("doi:",doi)
                           if(!is_empty(pmcid)){
                             replacement = paste0("PMCID: ",pmcid,". ",replacement)
                           }
                           
                           if(!identical(pmid,character(0)) & identical(pmcid,character(0))){
                             replacement = paste0("PMID: ",pmid,". ",replacement)
                           }
                           
                           c = str_replace(c,original,replacement)
                           
                         }
                         return(c)
                         
                       }) %>% 
    unlist()
  
  return(cited_updated)
  
}

