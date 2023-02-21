library(dplyr)
library(leaflet)
library(jsonlite)

### Get the coordinates by requesting the taxo core of the solr database
GetCooFromTaxID <- function(taxids, map){
  if (map == "ncbi"){
    map <- "https://lifemap-ncbi.univ-lyon1.fr/solr/taxo/select?q=taxid%3A"
  } else if (map == "fr"){
    map <- "https://lifemap.univ-lyon1.fr/solr/taxo/select?q=taxid%3A"
  }
  taxids<-as.character(taxids) #change to characters.
  DATA<-NULL
  i<-1
  while(i<=length(taxids)) {
    taxids_sub<-taxids[i:(i+99)]
    taxids_sub<-taxids_sub[!is.na(taxids_sub)]
    taxids_sub<-paste(taxids_sub, collapse="%0Ataxid%3A")
    url<-paste(map,taxids_sub,"&wt=json&rows=1000",sep="", collapse="")
    #do the request :
    data_sub<-fromJSON(url)
    if (data_sub$response$numFound > 0){
      DATA<-rbind(DATA,data_sub$response$docs[,c("taxid","lon","lat", "sci_name","zoom", "nbdesc")])
    }
    i<-i+100
  }
  if (is.null(DATA) == FALSE){
    if ("0" %in% taxids){
      DATA <- rbind(DATA,c(0,0,-4.2265,"LUCA",0,1))
    }
    for (j in 1:ncol(DATA)) DATA[,j]<-unlist(DATA[,j])
    class(DATA$taxid)<-"character"
    class(DATA$lon)<-"numeric"
    class(DATA$lat)<-"numeric"

    not_found <- c()
    if (length(taxids) != nrow(DATA)){
      for (element in taxids){
        if (!(element %in% DATA$taxid)){
          not_found <- append(not_found,element)
        }
      }
    }

    if (length(not_found)>0){
      warning(sprintf("The following TaxIDs were not found in the database : %s",paste(not_found, collapse=",")))
    }
  }
  if (is.null(DATA)==TRUE){
    stop("None of the TaxIDs given were found in the database")
  } else {
    return(DATA)
  }
}

### Get the lineage, interrogate the addi core of the solr database
GetLinFromTaxID <- function(taxids,map){
  if (map == "ncbi"){
    map <- "https://lifemap-ncbi.univ-lyon1.fr/solr/addi/select?q=taxid%3A"
  } else if (map == "fr"){
    map <- "https://lifemap.univ-lyon1.fr/solr/addi/select?q=taxid%3A"
  }
  taxids<-as.character(taxids) #change to characters.
  DATA<-NULL
  i<-1
  while(i<=length(taxids)) {
    taxids_sub<-taxids[i:(i+99)]
    taxids_sub<-taxids_sub[!is.na(taxids_sub)]
    taxids_sub<-paste(taxids_sub, collapse="%0Ataxid%3A")
    url<-paste(map,taxids_sub,"&wt=json&rows=1000",sep="", collapse="")
    #do the request :
    data_sub<-fromJSON(url)
    if (data_sub$response$numFound > 0){
      DATA<-rbind(DATA,data_sub$response$docs[,c("taxid","ascend","genomes")])
    }
    i<-i+100
  }
  class(DATA$taxid) <- "character"
  class(DATA$genomes) <- "numeric"
  return(DATA)
}

## get the common ancestors of all the taxa as well as the taxa path from the leaf to the ancestor
get_ancestry <- function(ids,map){
  data <- GetCooFromTaxID(ids,map)
  data_lineage <- GetLinFromTaxID(ids,map)
  data_final <- left_join(data, data_lineage, by = "taxid")

  global_ancestry <- apply(data_final[2:nrow(data_final),], function(x){
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

  ancestry_first_tax <- c(as.numeric(data_final[1,"taxid"]))
  for (element in data_final[1,"ascend"][[1]]){
    ancestry_first_tax <- append(ancestry_first_tax,element)
    if(element %in% lca){
      lca <- setdiff(lca,element)
    }
    if(length(lca) == 0){
      break
    }
  }
  global_ancestry$'1' <- list(ancestry_first_tax)
  return(global_ancestry)
}

## setting the color for special nodes
### the ids requested
### ancestors common to several of the ids
getColor <- function(id, taxids, ancestors, taxidscol, ancestorscol) {
  if (id %in% taxids){
    taxidscol
  } else if (id %in% ancestors){
    ancestorscol
  }
}

## add the lines between the taxa in 'df' on 'map'
add_ancestry <- function(df, map, ids, ancestors, linecolor, taxidscol,ancestorscol){
  for (row in 1:nrow(df)){
    map <- map %>% addPolylines(lng=c(df[row,"lon.x"],df[row,"lon.y"]), lat=c(df[row,"lat.x"],df[row,"lat.y"]), color=linecolor) %>%
      addCircleMarkers(lng=c(df[row,"lon.x"],df[row,"lon.y"]), lat=c(df[row,"lat.x"],df[row,"lat.y"]), label=c(df[row,"tax1"],df[row,"tax2"]), fillColor = "grey", stroke=FALSE)

    if (df[row,"tax1"] %in% ids | df[row,"tax1"] %in% ancestors){
      icons <- awesomeIcons(
        icon = 'ios-close',
        iconColor = 'black',
        library = 'ion',
        markerColor = getColor(df[row,"tax1"], ids, ancestors, taxidscol,ancestorscol)
      )
      map <- map %>%
        addAwesomeMarkers(lng=df[row,"lon.x"], lat=df[row,"lat.x"], icon=icons, label=df[row,"tax1"])
    }
    if (df[row,"tax2"] %in% ids | df[row,"tax2"] %in% ancestors){
      icons <- awesomeIcons(
        icon = 'ios-close',
        iconColor = 'black',
        library = 'ion',
        markerColor = getColor(df[row,"tax2"], ids, ancestors, taxidscol,ancestorscol)
      )
      map <- map %>%
        addAwesomeMarkers(lng=df[row,"lon.y"], lat=df[row,"lat.y"], icon=icons, label=df[row,"tax2"])
    }
  }
  return(map)
}

## get the closest ancestors shared by multiple taxa
get_shared_ancestors <- function(df){
  ancestors <- append(as.vector(df$tax1),as.vector(df$tax2))
  ancestors <- as.data.frame(table(ancestors))
  ancestors <- as.vector(ancestors[ancestors$Freq > 2 | ancestors$ancestors == 0,]$ancestors)
  return(ancestors)
}


#' Trace the ancestry of multiple taxa
#'
#' @param ids a list of TaxID to visualise
#' @param map the base map wanted
#' @param linecolor the color of the line tracing the ancestry
#'
#' @return a Leaflet map
#' @export
#'
#' @examples trace_ancestry(c(3641,3649,403667,3394,54308,1902823), "fr")
trace_ancestry <- function(ids, map="ncbi", linecolor="red", taxidscol="green",ancestorscol="red"){
  if (length(ids)==1){
    coo_ids <- GetCooFromTaxID(ids, map)
    icons <- awesomeIcons(
      icon = 'ios-close',
      iconColor = 'black',
      library = 'ion',
      markerColor = taxidscol
    )
    m <- display_map(df=coo_ids,map=map) %>% addAwesomeMarkers(~lon, ~lat, icon=icons, label=~sci_name)
    return(m)
  } else {

    ancestry <- get_ancestry(ids,map)
    # couple of father-son nodes (to get the correct order when drawing lines)
    ancestry_couples <- lapply(ancestry, function(x){
      trace <- list()
      for (i in 2:length(x[[1]])-1){
        trace[[as.character(i)]] <- sort(c(x[[1]][i],x[[1]][i+1]))
      }
      return(trace)
    })

    # converting the list into a dataframe
    df_ancestry <- t(as.data.frame(ancestry_couples))
    df_ancestry <- unique(df_ancestry)
    colnames(df_ancestry) <- c("tax1", "tax2")

    # Getting the coordinates for all
    tax_coo <- GetCooFromTaxID(unique(c(as.vector(df_ancestry[,1]),as.vector(df_ancestry[,2]))),map)

    df_ancestry <- merge(df_ancestry,tax_coo, by.x="tax1", by.y="taxid")
    df_ancestry <- merge(df_ancestry,tax_coo, by.x="tax2", by.y="taxid")

    ancestors <- get_shared_ancestors(df_ancestry)

    m <- display_map(map=map)
    add_ancestry(df_ancestry,m, ids, ancestors, linecolor,taxidscol,ancestorscol)
  }

}
