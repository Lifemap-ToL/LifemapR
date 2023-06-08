#' Check if the URL is working
#'
#' @param basemap_url the url corresponding to the basemap interrogated
#' @param t the time before timeout
#'
#' @return a logical
url_verification <- function(basemap_url, t = 20){
  url_in <- paste(basemap_url, "#/taxo/query", sep = "", collapse = "")
  con <- url(url_in)
  check <- suppressWarnings(try(open.connection(con, open = "rt", timeout = t), silent = T)[1])
  suppressWarnings(try(close.connection(con), silent = T))
  ifelse(is.null(check), TRUE, FALSE)
}


#' Request one of the core of the solr database corresponding to the basemap choosen with TaxIDs wanted
#'
#' @param taxids a vector of TaxIDs to be requested
#' @param basemap the name of the basemap wanted to represent the datas on (can be "ncbi", "fr" ...)
#'
#' @return a dataframe containing all the informations requested from the database
request_database_new <- function(taxids, basemap) {
  # getting the rigth URL depending on the basemap wanted
  if (basemap == "ncbi") {
    basemap_url <- "https://lifemap-ncbi.univ-lyon1.fr/solr/"
  } else if (basemap == "fr") {
    basemap_url <- "https://lifemap-fr.univ-lyon1.fr/solr/"
  } else if (basemap == "base") {
    basemap_url <- "https://lifemap.univ-lyon1.fr/solr/"
  } else if (basemap == "virus") {
    basemap_url <- "http://virusmap.univ-lyon1.fr/solr/"
  } else {stop(sprintf('%s is not a working url, try c("base", "fr", "ncbi" or "virus")', basemap))}

  if (url_verification(basemap_url = basemap_url) == FALSE) {
    basemaps <- c("base", "ncbi", "fr", "virus")
    stop(sprintf("the url you are trying is not working, why not try these one : %s", paste(basemaps[!(basemaps %in% basemap)], collapse = ", ")))
  }

  DATA <- NULL
  i <- 1
  while (i <= length(taxids)) {
    #requests are made 100 by 100
    taxids_sub <- taxids[i:(i + 99)]
    taxids_sub <- taxids_sub[!is.na(taxids_sub)]
    # adding the URL separator between the taxids
    taxids_sub <- paste(taxids_sub, collapse = "%0Ataxid%3A")

    url <- paste(basemap_url, "taxo", "/select?q=taxid%3A",
                 taxids_sub, "&wt=json&rows=1000",
                 sep = "", collapse = ""
    )

    # doing the request :
    data_sub <- jsonlite::fromJSON(url)
    if (data_sub$response$numFound > 0) {
      DATA <- rbind(DATA, data_sub$response$docs[, c("taxid", "lon", "lat", "sci_name", "zoom", "ascend")])
    }
    i <- i + 100
  }

  # formating the dataframe
  if (!(is.null(DATA))) {
    for (j in 1:(ncol(DATA)-1)) DATA[, j] <- unlist(DATA[, j])
    class(DATA$lon) <- "numeric"
    class(DATA$lat) <- "numeric"
    class(DATA$zoom) <- "numeric"
    class(DATA$taxid) <- "character"
  }
  return(DATA)
}


#' A function to construct a LifemapR object, usable by other functions
#'
#' @param df a dataframe containing at least a column named "taxid"
#' @param basemap the basemap wanted ("fr","ncbi", "base" or "virus")
#' @param verbose if TRUE, will write informations in the terminal
#'
#' @return a lifemap object with :
#' - df : a dataframe containing at least :
#'   - the taxids (taxid)
#'   - the longitude (long)
#'   - the latitude (lat)
#'   - the scientific nam (sci_name)
#'   - the zoom at which the taxa can be seen (zoom)
#'   - the ascendants of requested taxids (ascend)
#'   - the type of each taxid ("requested" or "ancestor")
#' - basemap : the basemap used to get taxa's informations
#'
#' @importFrom jsonlite fromJSON
#' @importFrom dplyr bind_rows distinct
#'
#' @export
#' @examples
#' data(eukaryotes_1000)
#' LM <- build_Lifemap_new(eukaryotes_1000, "ncbi")
build_Lifemap_new <- function(df, basemap = c("ncbi","fr", "base","virus"), verbose=TRUE) {
  basemap <- match.arg(arg = basemap, choices = basemap)

  if (is.null(df$taxid)) {
    stop('The dataframe must at least contain a "taxid" column')
  }

  df_distinct <- dplyr::distinct(df,taxid, .keep_all = TRUE)
  if (!(nrow(df_distinct) == nrow(df))) {
    warning(sprintf("%s duplicated TaxIDs were removed \n",nrow(df)-nrow(df_distinct)))
  }


  # get coordinates
  if (verbose){
    cat("requesting the taxids ...\n")
  }
  DATA_REQUEST <- request_database_new(taxids = unique(df_distinct$taxid), basemap)

  if (is.null(DATA_REQUEST)) {
    stop("None of the TaxIDs given were found in the database")
  }

  # if the root was in the requested taxids, re add it now so as not to lose any information
  if (0 %in% df$taxid) {
    # not_found <- not_found[not_found != 0]
    LUCA <- data.frame(taxid = "0", lon = 0, lat = -4.226497, zoom = 5)
    DATA_REQUEST <- dplyr::bind_rows(DATA_REQUEST, LUCA)
  }

  if (nrow(df_distinct) != nrow(DATA_REQUEST)) {
    not_found <- setdiff(df_distinct$taxid, DATA_REQUEST$taxid)
    if (length(not_found) == 1) {
      warning(sprintf(
        "%s TaxID was not found. The following TaxID was not found in the database : %s",
        length(not_found), paste(not_found, collapse = ",")
      ))
    } else if (length(not_found) > 0) {
      warning(sprintf(
        "%s TaxIDs were not found. The following TaxIDs were not found in the database : %s",
        length(not_found), paste(not_found, collapse = ",")
      ))
    }
  }

  INFOS_DATA <- merge(df_distinct, DATA_REQUEST, by.x = "taxid", by.y = "taxid")
  class(INFOS_DATA$taxid) <- "character"

  # get the coordinates of the ancestors of the taxids
  if (verbose){
    cat("constructing the ancestry ...\n")
  }
  unique_ancestors <- unique(unlist(DATA_REQUEST$ascend))

  # we don't request already requested taxid
  tax_request <- as.vector(INFOS_DATA$taxid)
  real_ancestors <- setdiff(unique_ancestors,tax_request)

  ANCESTORS <- request_database_new(taxids = real_ancestors, basemap)

  ANCESTORS$type <- "ancestor"
  INFOS_DATA$type <- "requested"

  # nodes_requested <- ANCESTORS[ANCESTORS$taxid %in% INFOS_DATA$taxid,]

  INFOS_DATA <- dplyr::bind_rows(INFOS_DATA, ANCESTORS)
  LUCA <- INFOS_DATA[INFOS_DATA$taxid == "0",]



  if (!(0 %in% df$taxid)) {
    INFOS_DATA$ancestor <- sapply(INFOS_DATA$ascend, "[[", 1)
    LUCA <- data.frame(taxid = "0", lon = 0, lat = -4.226497, zoom = 5, type = "ancestor", ancestor = NA)
    INFOS_DATA <- dplyr::bind_rows(INFOS_DATA, LUCA)
  } else {
    INFOS_DATA <- INFOS_DATA[order(INFOS_DATA$taxid),]
    INFOS_DATA$ancestor <- sapply(INFOS_DATA$ascend, "[[", 1)
    INFOS_DATA$ancestor <- c(NA, unlist(INFOS_DATA$ancestor))
  }

  # FINAL_DATA <- dplyr::bind_rows(INFOS_DATA, LUCA)

  lm_obj <- list(df = INFOS_DATA, basemap = basemap)
  class(lm_obj) <- c("lifemap_obj", "list")

  return(lm_obj)
}

