### Get the coordinates
GetCooFromTaxID <- function(taxids){
  taxids<-as.character(taxids) #change to characters.
  DATA<-NULL
  i<-1
  while(i<=length(taxids)) {
    taxids_sub<-taxids[i:(i+99)]
    taxids_sub<-taxids_sub[!is.na(taxids_sub)]
    taxids_sub<-paste(taxids_sub, collapse="%0Ataxid%3A") 
    url<-paste("https://lifemap-ncbi.univ-lyon1.fr/solr/taxo/select?q=taxid%3A",taxids_sub,"&wt=json&rows=1000",sep="", collapse="")
    #do the request :
    data_sub<-fromJSON(url)
    DATA<-rbind(DATA,data_sub$response$docs[,c("taxid","lon","lat", "sci_name","zoom", "nbdesc")])
    i<-i+100
  }
  if ("0" %in% taxids){
    DATA <- rbind(DATA,c(0,0,-4.2265,"LUCA",0,1))
  }
  for (j in 1:ncol(DATA)) {DATA[,j]<-unlist(DATA[,j])}
  class(DATA$taxid)<-"character"
  class(DATA$lon)<-"numeric"
  class(DATA$lat)<-"numeric"
  return(DATA)
}

### Get the lineage, interrogate the addi core of the solr database
GetLinFromTaxID <- function(taxids){
  taxids<-as.character(taxids) #change to characters.
  DATA<-NULL
  i<-1
  while(i<=length(taxids)) {
    taxids_sub<-taxids[i:(i+99)]
    taxids_sub<-taxids_sub[!is.na(taxids_sub)]
    taxids_sub<-paste(taxids_sub, collapse="%0Ataxid%3A") 
    url<-paste("https://lifemap-ncbi.univ-lyon1.fr/solr/addi/select?q=taxid%3A",taxids_sub,"&wt=json&rows=1000",sep="", collapse="")
    #do the request :
    data_sub<-fromJSON(url)
    DATA<-rbind(DATA,data_sub$response$docs[,c("taxid","ascend","genomes")])
    i<-i+100
  }
  class(DATA$taxid) <- "character"
  class(DATA$genomes) <- "numeric"
  return(DATA)
}

## get the comon ancestors of all the taxas
get_ancestry <- function(ids){
  data <- GetCooFromTaxID(ids)
  data_lineage <- GetLinFromTaxID(ids)
  data_final <- left_join(data, data_lineage, by = "taxid")
  
  global_ancestry <- apply(data_final[2:nrow(data_final),], function(x){
    # x$ascend
    # x$taxid
    ancestry <- c(as.numeric(x$taxid))
    for (element in x$ascend){
      ancestry <- append(ancestry,element)
      if (element %in% data_final[1,"ascend"][[1]]){
        element
        break
      }
    }
    return(list(ancestry,element))
  }, MARGIN = 1)

  lca <- c()
  for (element in global_ancestry){
    lca <- append(lca,element[[2]])
  }
  
  ancestry <- c(as.numeric(data_final[1,"taxid"]))
  for (element in data_final[1,"ascend"][[1]]){
    ancestry <- append(ancestry,element)
    if(element %in% lca){
      lca <- setdiff(lca,element)
    }
    if(length(lca) == 0){
      break
    }
  }
  global_ancestry$'1' <- list(ancestry)
  return(global_ancestry)
}

get_ancestry_infos <- function(ancestry_info){
  ancestry_trace <- lapply(ancestry_info, function(x){
    trace <- list()
    for (i in 2:length(x[[1]])-1){
      print(c(x[[1]][i], x[[1]][i+1]))
      trace[[as.character(i)]] <- GetCooFromTaxID(c(x[[1]][i],as.character(x[[1]][i+1])))
    }
    return(trace)
  })
}

add_ancestry <- function(list_of_df, map){
  for (element in list_of_df){
    print("probleme")
    print(element)
    for (i in element){
      map <- map %>% addPolylines(i$lon, i$lat, color="red") %>%
        addCircleMarkers(i$lon, i$lat, label=i$sci_name, fillColor = "red")
    }
  }  
  return(map)
}

#' Trace the ancestry of multiple taxa
#'
#' @param ids a list of TaxID to visualise
#'
#' @return a Leaflet map
#' @export
#'
#' @examples trace_ancestry(c(3641,3649,403667,3394,2))
trace_ancestry <- function(ids){
  ancestry <- get_ancestry(ids)
  ancestry_infos <- get_ancestry_infos(ancestry)
  m <- display_map()
  add_ancestry(ancestry_infos,m)
}
