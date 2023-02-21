library(dplyr)
library(leaflet)
library(jsonlite)

## The df must contain at least a column named taxid
request_database <- function(df,basemap){
  # getting the rigth URL depending on the basemap wanted
  if (basemap == "ncbi"){
    db_taxo <- "https://lifemap-ncbi.univ-lyon1.fr/solr/taxo/select?q=taxid%3A"
    db_addi <- "https://lifemap-ncbi.univ-lyon1.fr/solr/addi/select?q=taxid%3A"
  } else if (basemap == "fr"){
    db_taxo <- "https://lifemap.univ-lyon1.fr/solr/taxo/select?q=taxid%3A"
    db_addi <- "https://lifemap.univ-lyon1.fr/solr/addi/select?q=taxid%3A"
  }

  taxids <- as.character(df$taxids)
  COO <- NULL
  LIN <- NULL
  i<-1
  while(i<=length(taxids)) {
    taxids_sub<-taxids[i:(i+99)]
    taxids_sub<-taxids_sub[!is.na(taxids_sub)]
    taxids_sub<-paste(taxids_sub, collapse="%0Ataxid%3A")
    
    url_taxo <-paste(db_taxo,taxids_sub,"&wt=json&rows=1000",sep="", collapse="")
    url_addi <-paste(db_addi,taxids_sub,"&wt=json&rows=1000",sep="", collapse="")
    
    #do the request :
    coo_sub<-fromJSON(url_taxo)
    lin_sub<-fromJSON(url_addi)
    if (coo_sub$response$numFound > 0){
      COO <-rbind(COO,coo_sub$response$docs[,c("taxid","lon","lat", "sci_name","zoom")])
    }
    if (lin_sub$response$numFound > 0){
      LIN <-rbind(LIN,lin_sub$response$docs[,c("taxid","ascend","genomes")])
    }
    i<-i+100
  }
  DATA <- merge(df,COO, by.x="taxids", by.y="taxid")
  DATA <- merge(DATA,LIN, by.x="taxids", by.y="taxid")
  
  ancestors <- unique(unlist(DATA$ascend))
  
  return(ancestors)
}