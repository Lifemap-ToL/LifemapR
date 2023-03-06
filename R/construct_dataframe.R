#' Request one of the core of the solr database corresponding to the basemap choosen with TaxIDs
#'
#' @param taxids a vector of TaxIDs to be requested
#' @param basemap the name of the basemap wanted to represent the datas on, either "ncbi" or "fr"
#' @param core The core to be requested either "taxo" or "addi"
#'
#' @return a dataframe containing all the informations from the database
request_database <- function(taxids, basemap, core) {
  # getting the rigth URL depending on the basemap wanted
  if (basemap == "ncbi") {
    basemap_url <- "https://lifemap-ncbi.univ-lyon1.fr/solr/"
  } else if (basemap == "fr") {
    basemap_url <- "https://lifemap.univ-lyon1.fr/solr/"
  }

  DATA <- NULL
  i <- 1
  while (i <= length(taxids)) {
    taxids_sub <- taxids[i:(i + 99)]
    taxids_sub <- taxids_sub[!is.na(taxids_sub)]
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




#' Create a df with informations about ancestry (father and son nodes)
#'
#' @param df a dataframe
#'
#' @return a dataframe
#'
#' @importFrom dplyr bind_rows
#'
get_direct_ancestor <- function(df) {
  ancestors <- c()
  descendants <- c()
  df_ancestry <- data.frame("descendant"=numeric(0), "ancestor"=numeric(0))

  for (taxid in 1:nrow(df[df$type=="requested",])) {
    ancestry <- unlist(df[taxid, "ascend"])
    df_ancestry <- rbind(df_ancestry,c(as.numeric(df[taxid, "taxid"]), as.numeric(ancestry[1])))
    ancestors <- append(ancestors,ancestry[1])
    descendants <- append(descendants,df[taxid, "taxid"])

    for (id in 1:(length(ancestry)-1)) {
      descendant <- ancestry[id]
      ancestor <- ancestry[id+1]

      if (!(ancestor %in% ancestors)) {
        df_ancestry <- rbind(df_ancestry,c(descendant, ancestor))
        ancestors <- append(ancestors,ancestor)
        descendants <- append(descendants, descendant)
      } else {
        if (!(descendant %in% descendants)) {
          df_ancestry <- rbind(df_ancestry,c(descendant, ancestor))
          descendants <- append(descendants, descendant)
        } else {
          break
        }
      }
    }
  }
  colnames(df_ancestry) <- c("descendant", "ancestor")
  df_tot <- merge(df, df_ancestry, by.x = "taxid", by.y = "descendant")
  return(df_tot)
}




#' A function to construct a dataframe usable by LifemapR functions
#'
#' @param df a dataframe containing at least a column named "taxid"
#' @param basemap the basemap wanted, either "fr" or "ncbi" ("ncbi" by default)
#'
#' @return a list with :
#' - a dataframe containing at least :
#'   - the taxids (taxid)
#'   - the longitude (long)
#'   - the latitude (lat)
#'   - the scientific nam (sci_name)
#'   - the zoom at which the taxa can be seen (zoom)
#'   - the ascendants of requested taxids (ascend)
#'   - the type of each taxid ("requested" or "ancestor")
#' - the basemap used
#'
#' @importFrom jsonlite fromJSON
#' @importFrom dplyr bind_rows distinct
#'
#' @export
#'
#' @examples
#' df <- data.frame(
#'   "taxid" = c(3641, 3649, 403667, 3394, 54308, 1902823),
#'   "info1" = c(3, 3, 4, 3, 5, 1)
#' )
#'
#' Lifemap_df <- construct_dataframe(df, "ncbi")
construct_dataframe <- function(df, basemap = "ncbi") {
  if (is.null(df$taxid)) {
    stop('The dataframe must at least contain a "taxid" column')
  }

  df <- distinct(df)

  # get coordinates
  print("getting the coordinates ...")
  COO <- request_database(taxids = df$taxid, basemap, "taxo")

  if (is.null(COO)) {
    stop("None of the TaxIDs given were found in the database")
  }

  not_found <- c()
  if (nrow(df) != nrow(COO)) {
    not_found <- c()
    for (id in df$taxid) {
      if (!(id %in% COO$taxid)) {
        not_found <- append(not_found, id)
      }
    }
  }
  if (length(not_found) > 0) {
    warning(sprintf(
      "%s TaxIDs were not found. The following TaxIDs were not found in the database : %s",
      length(not_found),paste(not_found, collapse = ",")
    ))
  }

  # get lineage informations
  print("getting the lineage ...")
  LIN <- request_database(taxids = df$taxid, basemap, "addi")
  DATA <- merge(COO, LIN, by.x = "taxid", by.y = "taxid")

  INFOS_DATA <- merge(df, DATA, by.x = "taxid", by.y = "taxid")
  class(INFOS_DATA$taxid) <- "character"

  # get the coordinates of the ancestors of the taxids
  print("constructing the ancestry ...")
  unique_ancestors <- unique(unlist(DATA$ascend))

  # we don't request already requested taxid
  tax_request <- as.vector(DATA$taxid)
  real_ancestors <- setdiff(unique_ancestors,tax_request)

  ANCESTORS <- request_database(taxids = real_ancestors, basemap, "taxo")

  ANCESTORS$type <- "ancestor"
  INFOS_DATA$type <- "requested"

  nodes_requested <- ANCESTORS[ANCESTORS$taxid %in% INFOS_DATA$taxid,]

  INFOS_DATA <- dplyr::bind_rows(INFOS_DATA, ANCESTORS)

  print("getting the direct ancestor ...")
  FINAL_DATA <- get_direct_ancestor(INFOS_DATA)

  LUCA <- data.frame(taxid="0",lon=0, lat=-4.226497,sci_name="Luca",zoom=5, type="ancestor")

  FINAL_DATA <- dplyr::bind_rows(FINAL_DATA, LUCA)

  return(list(FINAL_DATA,basemap))
}
