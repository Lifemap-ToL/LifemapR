#' Request one of the core of the solr database corresponding to the basemap choosen with TaxIDs wanted
#'
#' @param taxids a vector of TaxIDs to be requested
#' @param basemap the name of the basemap wanted to represent the datas on (can be "ncbi", "fr" ...)
#' @param core The core to be requested either "taxo" (for position informations) or "addi" (ascendance informations)
#'
#' @return a dataframe containing all the informations requested from the database
request_database <- function(taxids, basemap, core) {
  # getting the rigth URL depending on the basemap wanted
  if (basemap == "ncbi") {
    basemap_url <- "https://lifemap-ncbi.univ-lyon1.fr/solr/"
  } else if (basemap == "fr") {
    basemap_url <- "https://lifemap-fr.univ-lyon1.fr/solr/"
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


#' Add the direct ancestor for each taxa of the dataframe
#'
#' @param df a dataframe with requested taxid and the full ascendance information
#'
#' @return a dataframe with the direct ancestor for each taxa
#'
#' @importFrom dplyr bind_rows
get_direct_ancestor <- function(df) {
  ancestors <- c()
  descendants <- c()
  df_ancestry <- data.frame("descendant"=numeric(0), "ancestor"=numeric(0))

  for (taxid in 1:nrow(df[df$type=="requested",])) {
    ancestry <- unlist(df[taxid, "ascend"])

    # add the requested taxa and ancestor if not already encountered
    if (!(as.numeric(df[taxid, "taxid"])) %in% descendants) {
      df_ancestry <- rbind(df_ancestry,c(as.numeric(df[taxid, "taxid"]),
                                         as.numeric(ancestry[1])))
      ancestors <- append(ancestors,ancestry[1])
      descendants <- append(descendants,df[taxid, "taxid"])
    }

    for (id in 1:(length(ancestry)-1)) {
      descendant <- ancestry[id]
      ancestor <- ancestry[id+1]

      # add descendant and ancestor if ancestor not already encountered
      if (!(ancestor %in% ancestors)) {
        df_ancestry <- rbind(df_ancestry,
                             c(descendant, ancestor))
        ancestors <- append(ancestors,ancestor)
        descendants <- append(descendants, descendant)
      } else {

        # add descendant if not already encountered and ancestor already known
        if (!(descendant %in% descendants)) {
          df_ancestry <- rbind(df_ancestry,
                               c(descendant, ancestor))
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




#' A function to construct a LidemapR object, usable by other functions
#'
#' @param df a dataframe containing at least a column named "taxid"
#' @param basemap the basemap wanted (can be "ncbi", "fr" ...)
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
#'
#' @examples
#' df <- read.csv(file="data/eukaryotes_1000.txt", sep="\t", header=TRUE)
#'
#' Lifemap_df <- build_Lifemap(df, "fr")
build_Lifemap <- function(df, basemap = "fr", verbose=TRUE) {
  if (is.null(df$taxid)) {
    stop('The dataframe must at least contain a "taxid" column')
  }

  df_distinct <- dplyr::distinct(df,taxid, .keep_all = TRUE)
  if (!(nrow(df_distinct) == nrow(df))) {
    warning(sprintf("%s rows have been removed because there was duplicated TaxIDs \n",nrow(df)-nrow(df_distinct)))
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
  if (length(not_found) > 0) {
    warning(sprintf(
      "%s TaxIDs were not found. The following TaxIDs were not found in the database : %s",
      length(not_found),paste(not_found, collapse = ",")
    ))
  }

  if (verbose){
    cat("getting the lineage ...\n")
  }
  LIN <- request_database(taxids = unique(df_distinct$taxid), basemap, "addi")
  DATA <- merge(COO, LIN, by.x = "taxid", by.y = "taxid")

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

  if (verbose){
    cat("creating the final dataframe ...\n")
  }
  FINAL_DATA <- get_direct_ancestor(INFOS_DATA)

  LUCA <- data.frame(taxid="0",lon=0, lat=-4.226497,sci_name="Luca",zoom=5, type="ancestor")

  FINAL_DATA <- dplyr::bind_rows(FINAL_DATA, LUCA)

  lm_obj <- list(df=FINAL_DATA,basemap=basemap)
  class(lm_obj) <- c("lifemap.obj","list")

  return(lm_obj)
}

#' print lifemap.obj objects
#'
#'
#' @description When the LifemapR object is just created, give the number of columns and rows
#' as well as the basemap used to get the datas form the database
#'
#' @param lm_obj
#'
#' @return NA
#' @export
#'
#' @examples print(LM_df)
print.lifemap.obj <- function(lm_obj) {
  if (is.null(lm_obj$aes)) {
    cat('The dataframe contains', nrow(lm_obj$df),'rows and', ncol(lm_obj$df), 'columns. \n')
    cat('The basemap used is :', lm_obj$basemap,'\n')
  } else {
    print(draw_markers(lm_obj))
  }

}

is.lifemap.obj <- function(x) inherits(x, "lifemap.obj")
