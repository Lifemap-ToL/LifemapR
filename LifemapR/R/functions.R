#' Create a Lifemap base
#'
#' @description This function create a blank Leaflet map using the leaflet function from the leaflet package.
#' if a dataframe is provided, it will be used for the creation of the map object
#'
#' @param map The base choose to display, either \itemize{
#' - 'fr' for \url{https://lifemap-fr.univ-lyon1.fr/}
#' - 'ncbi' for \url{https://lifemap-ncbi.univ-lyon1.fr/} }
#'
#' @return HTML widget object  with graphics layers
#' @export
#'
#' @examples display_map("fr")
display_map <- function(df=NULL,map="ncbi") {
  if (map == "fr"){
    display="http://lifemap.univ-lyon1.fr/osm_tiles/{z}/{x}/{y}.png"
  } else if (map == "ncbi"){
    display="http://lifemap-ncbi.univ-lyon1.fr/osm_tiles/{z}/{x}/{y}.png"
    }
  leaflet::leaflet(df) %>%
    leaflet::addTiles(display)
}

#' Get coordinates from SolR database
#' 
#' @description This function return a dataframe with the coordinates on the lifemap for each ID 
#'
#' @param list_IDs a vector of TaxIDs you want to represent
#'
#' @return a dataframe
#' @export
#'
#' @examples get_coordinates(c(2,9443,2087))
get_coordinates <- function(list_IDs){
  list_IDs<-as.character(list_IDs) #change to characters.
  DATA<-NULL
  i<-1
  while(i<=length(list_IDs)) {
    taxids<-list_IDs[i:(i+9)]
    taxids<-taxids[!is.na(taxids)]
    taxids<-paste(taxids, collapse="%0Ataxid%3A") #accepted space separator in url
    url<-paste("https://lifemap-ncbi.univ-lyon1.fr/solr/taxo/select?q=taxid%3A",taxids, sep="")
    data_sub<-fromJSON(url)
    if (data_sub$response$numFound > 0){
      DATA<-rbind(DATA,data_sub$response$docs[,c("taxid","lon","lat", "sci_name","zoom")])
    }
    i<-i+10
  }
  if (is.null(DATA) == FALSE){
    for (j in 1:ncol(DATA)) DATA[,j]<-unlist(DATA[,j])
    class(DATA$taxid)<-"character"
    return(DATA)
  } else {
    return("empty dataframe")
  }
}