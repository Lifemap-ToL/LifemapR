#' Check if the URL is working
#'
#' @param basemap_url The url corresponding to the basemap interrogated.
#' @param t The time before timeout.
#'
#' @return A logical.
url_verification <- function(basemap_url, t = 20){
  url_in <- paste(basemap_url, "#/taxo/query", sep = "", collapse = "")
  con <- url(url_in)
  check <- suppressWarnings(try(open.connection(con, open = "rt", timeout = t), silent = T)[1])
  suppressWarnings(try(close.connection(con), silent = T))
  ifelse(is.null(check), TRUE, FALSE)
}


#' Request one of the core of the solr database corresponding to the basemap choosen with the TaxIDs wanted.
#'
#' @param taxids A vector of TaxIDs to be requested.
#' @param basemap The name of the basemap on which the data is represented (can be "ncbi", "fr", "base" or "virus").
#' @param core The core to be requested either "taxo" (for position details) or "addi" (for ascendance details).
#'
#' @return A dataframe containing all the pieces of information requested from the database.
request_database <- function(taxids, basemap, core) {
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

    url <- paste(basemap_url, core, "/select?q=taxid%3A",
      taxids_sub, "&wt=json&rows=1000",
      sep = "", collapse = ""
    )

    # doing the request :
    data_sub <- jsonlite::fromJSON(url)
    if (data_sub$response$numFound > 0) {
      if (core == "taxo") {
        DATA <- rbind(DATA, data_sub$response$docs[, c("taxid", "lon", "lat", "sci_name", "zoom")])
      } else if (core == "addi") {
        DATA <- rbind(DATA, data_sub$response$docs[, c("taxid", "ascend", "genomes")])
      }
    }
    i <- i + 100
  }

  # formating the dataframe
  if (!(is.null(DATA))) {
    if (core == "taxo") {
      for (j in 1:ncol(DATA)) DATA[, j] <- unlist(DATA[, j])
      class(DATA$lon) <- "numeric"
      class(DATA$lat) <- "numeric"
      class(DATA$zoom) <- "numeric"
    } else if (core == "addi") {
      class(DATA$genomes) <- "numeric"
    }
    class(DATA$taxid) <- "character"
  }
  return(DATA)
}


#' Add the direct ancestor for each taxa of the dataframe.
#'
#' @param df A dataframe with requested taxid and the full ascendance information.
#'
#' @return A dataframe with the direct ancestor for each taxa.
#'
#' @importFrom purrr reduce
get_direct_ancestor <- function(df) {
  df_ancestry <- apply(df[df$type=="requested",],1,function(x) {
    vec <- c(as.numeric(x$taxid), unlist(x$ascend))
    cbind(vec[-length(vec)], vec[-1])
  })
  df_ancestry <- purrr::reduce(df_ancestry, rbind, by = colnames(df_ancestry)[2])
  df_ancestry <- unique(df_ancestry)
  colnames(df_ancestry) <- c("descendant", "ancestor")
  df_tot <- merge(df, df_ancestry, by.x = "taxid", by.y = "descendant")
}



#' A function to construct a LifemapR object, usable by the other functions of the package.
#'
#' @param df A dataframe containing at least a column named "taxid" including NCBI format TaxIDs.
#' The dataframe can also contain characteristics associated with those TaxIDs in separated columns.
#' @param basemap The basemap wanted ("fr","ncbi", "base" or "virus").
#' @param verbose If TRUE, will write details on the status of the operation in the terminal.
#'
#' @return A lifemap object with :
#' - df : a dataframe containing at least for each TaxID (taxid) :
#'   - The longitude (long)
#'   - The latitude (lat)
#'   - The scientific name (sci_name)
#'   - The zoom level at which the taxa can be seen (zoom)
#'   - Its ascendants (ascend)
#'   - Its type ("requested" or "ancestor")
#' - basemap : the basemap used to get taxa's details
#'
#' @importFrom jsonlite fromJSON
#' @importFrom dplyr bind_rows distinct
#' @importFrom rlang .data
#'
#' @export
#' @examples
#' data(eukaryotes_80)
#' LM <- build_Lifemap(eukaryotes_80, "fr")
build_Lifemap <- function(df, basemap = c("fr","ncbi", "base","virus"), verbose=TRUE) {
  basemap <- match.arg(arg = basemap, choices = basemap)

  if (is.null(df$taxid)) {
    stop('The dataframe must at least contain a "taxid" column')
  }

  df_distinct <- dplyr::distinct(df, .data$taxid, .keep_all = TRUE)
  if (!(nrow(df_distinct) == nrow(df))) {
    warning(sprintf("%s duplicated TaxIDs were removed \n",nrow(df)-nrow(df_distinct)))
  }


  # get coordinates
  if (verbose){
    cat("getting the coordinates ...\n")
  }
  COO <- request_database(taxids = unique(df_distinct$taxid), basemap, "taxo")

  if (is.null(COO)) {
    stop("None of the TaxIDs given were found in the database")
  }

  not_found <- c()
  if (nrow(df_distinct) != nrow(COO)) {
    not_found <- c()
    for (id in df_distinct$taxid) {
      if (!(id %in% COO$taxid)) {
        not_found <- append(not_found, id)
      }
    }
  }

  if (0 %in% df$taxid) {
    not_found <- not_found[not_found != 0]
  }

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

  if (verbose){
    cat("getting the lineage ...\n")
  }
  LIN <- request_database(taxids = unique(df_distinct$taxid), basemap, "addi")
  DATA <- merge(COO, LIN, by.x = "taxid", by.y = "taxid")

  # if the root was in the requested taxids, add it now so as not to lose any information
  if (0 %in% df$taxid) {
    LUCA <- data.frame(taxid="0",lon=0, lat=-4.226497,sci_name="Luca",zoom=5)
    DATA <- dplyr::bind_rows(DATA, LUCA)
  }

  INFOS_DATA <- merge(df_distinct, DATA, by.x = "taxid", by.y = "taxid")
  class(INFOS_DATA$taxid) <- "character"


  # get the coordinates of the ancestors of the taxids
  if (verbose){
    cat("constructing the ancestry ...\n")
  }
  unique_ancestors <- unique(unlist(DATA$ascend))

  # we don't request already requested taxid
  tax_request <- as.vector(DATA$taxid)
  real_ancestors <- setdiff(unique_ancestors,tax_request)

  ANCESTORS <- request_database(taxids = real_ancestors, basemap, "taxo")

  ANCESTORS$type <- "ancestor"
  INFOS_DATA$type <- "requested"

  # nodes_requested <- ANCESTORS[ANCESTORS$taxid %in% INFOS_DATA$taxid,]

  INFOS_DATA <- dplyr::bind_rows(INFOS_DATA, ANCESTORS)
  LUCA <- INFOS_DATA[INFOS_DATA$taxid == "0",]

  if (verbose){
    cat("creating the final dataframe ...\n")
  }
  FINAL_DATA <- get_direct_ancestor(INFOS_DATA)

  if (!(0 %in% df$taxid)) {
    LUCA <- data.frame(taxid="0",lon=0, lat=-4.226497,sci_name="Luca",zoom=5, type="ancestor")
  }

  FINAL_DATA <- dplyr::bind_rows(FINAL_DATA, LUCA)

  lm_obj <- list(df=FINAL_DATA,basemap=basemap)
  class(lm_obj) <- c("lifemap_obj","list")

  return(lm_obj)
}

