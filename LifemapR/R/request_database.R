library(dplyr)
library(leaflet)
library(jsonlite)

request_database <- function(taxids,basemap, core){
  # getting the rigth URL depending on the basemap wanted
  if (basemap == "ncbi"){
    basemap_url <- "https://lifemap-ncbi.univ-lyon1.fr/solr/"
  } else if (basemap == "fr"){
    basemap_url <- "https://lifemap.univ-lyon1.fr/solr/"
  }
  
  DATA <- NULL
  i<-1
  while(i<=length(taxids)) {
    taxids_sub<-taxids[i:(i+99)]
    taxids_sub<-taxids_sub[!is.na(taxids_sub)]
    taxids_sub<-paste(taxids_sub, collapse="%0Ataxid%3A")
    
    url <-paste(basemap_url,core,"/select?q=taxid%3A",taxids_sub,"&wt=json&rows=1000",sep="", collapse="")
    
    #do the request :
    data_sub<-fromJSON(url)
    if (data_sub$response$numFound > 0){
      if (core == "taxo"){
        DATA <- rbind(DATA,data_sub$response$docs[,c("taxid","lon","lat", "sci_name","zoom")])
      } else if (core == "addi"){
        DATA <-rbind(DATA,data_sub$response$docs[,c("taxid","ascend","genomes")])
      }
    }
    i<-i+100
  }
  class(DATA$taxid) <- "character"
  
  return(DATA)
}

## The df must contain at least a column named taxid
construct_dataframe <- function(df,basemap){
  
  COO <- request_database(taxids=df$taxids, basemap, "taxo")
  print(COO)
  
  LIN <- request_database(taxids=df$taxids, basemap, "addi")
  print(LIN)
  
  DATA <- merge(COO, LIN, by.x="taxid", by.y="taxid")
  print(DATA)
  
  
  # # getting the rigth URL depending on the basemap wanted
  # if (basemap == "ncbi"){
  #   db_taxo <- "https://lifemap-ncbi.univ-lyon1.fr/solr/taxo/select?q=taxid%3A"
  #   db_addi <- "https://lifemap-ncbi.univ-lyon1.fr/solr/addi/select?q=taxid%3A"
  # } else if (basemap == "fr"){
  #   db_taxo <- "https://lifemap.univ-lyon1.fr/solr/taxo/select?q=taxid%3A"
  #   db_addi <- "https://lifemap.univ-lyon1.fr/solr/addi/select?q=taxid%3A"
  # }
  # 
  # taxids <- as.character(df$taxids)
  # COO <- NULL
  # LIN <- NULL
  # i<-1
  # while(i<=length(taxids)) {
  #   taxids_sub<-taxids[i:(i+99)]
  #   taxids_sub<-taxids_sub[!is.na(taxids_sub)]
  #   taxids_sub<-paste(taxids_sub, collapse="%0Ataxid%3A")
  #   
  #   url_taxo <-paste(db_taxo,taxids_sub,"&wt=json&rows=1000",sep="", collapse="")
  #   url_addi <-paste(db_addi,taxids_sub,"&wt=json&rows=1000",sep="", collapse="")
  #   
  #   #do the request :
  #   coo_sub<-fromJSON(url_taxo)
  #   lin_sub<-fromJSON(url_addi)
  #   if (coo_sub$response$numFound > 0){
  #     COO <-rbind(COO,coo_sub$response$docs[,c("taxid","lon","lat", "sci_name","zoom")])
  #   }
  #   if (lin_sub$response$numFound > 0){
  #     LIN <-rbind(LIN,lin_sub$response$docs[,c("taxid","ascend","genomes")])
  #   }
  #   i<-i+100
  # }
  # DATA <- merge(df,COO, by.x="taxids", by.y="taxid")
  # DATA <- merge(DATA,LIN, by.x="taxids", by.y="taxid")
  # 
  # ancestors <- unique(unlist(DATA$ascend))
  # 
  # return(ancestors)
}