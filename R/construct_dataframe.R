## taxids = list of taxids to be requested
## basemap = the name of the basemap wanted to represent our data on (ncbi, fr)
## core = the core to be requested (taxo or addi)
### Request a core of a solr database with taxids
#' Request one of the core of the solr database corresponding to the basemap choosen with TaxIDs
#'
#' @param taxids a vector of TaxIDs to be requested
#' @param basemap the name of the basemap wanted to represent the datas on, either "ncbi" or "fr"
#' @param core The core to be requested either "taxo" or "addi"
#'
#' @return a dataframe containing all the informations from the database
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

    url <-paste(basemap_url,core,"/select?q=taxid%3A",
                taxids_sub,"&wt=json&rows=1000",
                sep="", collapse="")

    #doing the request :
    data_sub<-jsonlite::fromJSON(url)
    if (data_sub$response$numFound > 0){
      if (core == "taxo"){
        DATA <- rbind(DATA,data_sub$response$docs[,c("taxid","lon","lat", "sci_name","zoom")])
      } else if (core == "addi"){
        DATA <-rbind(DATA,data_sub$response$docs[,c("taxid","ascend","genomes")])
      }
    }
    i<-i+100
  }
  #formating the dataframe
  if (!(is.null(DATA))){
    if (core == "taxo"){
      for (j in 1:ncol(DATA)) DATA[,j]<-unlist(DATA[,j])
      class(DATA$lon) <- "numeric"
      class(DATA$lat) <- "numeric"
    } else if (core == "addi"){
      class(DATA$genomes) <- "numeric"
    }
    class(DATA$taxid) <- "character"
  }

  return(DATA)
}

#' A function to construct a dataframe usable by LifemapR functions
#'
#' @param df a dataframe containing at least a column named "taxid"
#' @param basemap the basemap wanted, either "fr" or "ncbi" ("ncbi" by default)
#'
#' @return a dataframe containing at least :
#'  - the taxids (taxid)
#'  - the longitude (long)
#'  - the latitude (lat)
#'  - the scientific nam (sci_name)
#'  - the zoom at which the taxa can be seen (zoom)
#'  - the ascendants of requested taxids (ascend)
#'  - the type of each taxid ("requested" or "ancestor")
#'
#' @importFrom jsonlite fromJSON
#' @importFrom dplyr bind_rows
#'
#' @export
#'
#' @examples
#' df <- data.frame("taxid"=c(3641,3649,403667,3394,54308,1902823),
#'                  "info1"=c(3,3,4,3,5,1))
#'
#' Lifemap_df <- construct_dataframe(df,"ncbi")
construct_dataframe <- function(df,basemap="ncbi"){
  if (is.null(df$taxid)){
    stop('The dataframe must at least contain a "taxid" column')
  }

  #get coordinates
  COO <- request_database(taxids=df$taxid, basemap, "taxo")

  if (is.null(COO)) {
    stop ("None of the TaxIDs given were found in the database")
  }

  not_found <- c()
  if(nrow(df) != nrow(COO)){
    not_found <- c()
    for (id in df$taxid){
      if (!(id %in% COO$taxid)){
        not_found <- append(not_found, id)
      }
    }
  }
  if (length(not_found) > 0){
    warning(sprintf("The following TaxIDs were not found in the database : %s",
                    paste(not_found, collapse=",")))
  }

  #get lineage informations
  LIN <- request_database(taxids=df$taxid, basemap, "addi")
  DATA <- merge(COO, LIN, by.x="taxid", by.y="taxid")

  # get the coordinates of the ancestors of the taxids
  unique_ancestors <- unique(unlist(DATA$ascend))
  ANCESTORS <- request_database(taxids = unique_ancestors, basemap, "taxo")

  FINAL_DATA <- merge(df,DATA, by.x="taxid", by.y="taxid")
  class(FINAL_DATA$taxid) <- "character"

  ANCESTORS$type <- "ancestor"
  FINAL_DATA$type <- "requested"

  FINAL_DATA <- dplyr::bind_rows(FINAL_DATA, ANCESTORS)

  return(FINAL_DATA)
}
