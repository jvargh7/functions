library(xml2)
library(tidyverse)
# pmid_numeric = 21607057
# webpage = get_webpage(pmid_numeric)
# # extract_abstract(webpage)
# extract_mesh(webpage)
# extract_affiliations(webpage)
# extract_chemicals(webpage)

get_webpage <- function(pmid_numeric){
  
  address = paste0("https://eutils.ncbi.nlm.nih.gov/entrez/eutils/efetch.fcgi?db=pubmed&id=",pmid_numeric,"&retmode=XML&rettype=abstract");
  webpage = as_list(read_xml(address))$PubmedArticleSet$PubmedArticle$MedlineCitation
  
  return(webpage)
}
