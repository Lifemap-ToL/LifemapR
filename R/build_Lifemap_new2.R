#' Transform a string returned by a request into a usable dataframe
#'
#' @param string a string returned by
#'
#' @return a dataframe
#' @export
#'
#' @examples
formating <- function(string){
  str_tmp <- stri_split(string, regex = "\\[\n      \\{\n")[[1]][2]
  str_tmp <- stri_replace_all(str_tmp, ";", regex = 'taxid|zoom|lat|lon|ascend')
  str_tmp <- stri_replace_all(str_tmp, "", regex = ' |\\\"|:\\[|\\],\\\n|\\\n|\\{|\\},|\\]|\\}')
  str_tmp <- substring(str_tmp, 2)
  df <- read.table(text = str_tmp, col.names=c('taxid', 'zoom', 'lat', 'lon', 'ascend'), sep = ";")
  return(df)
}

good_request <- function(string){
  str_tmp <- stri_split(string, regex = "\\[\n      \\{\n")[[1]][1]
  str_tmp <- stri_split(str_tmp, regex = 'numFound\\\":|,\\\"start')
  if (as.numeric(str_tmp[[1]][2]) > 0){
    return(TRUE)
  } else {return(FALSE)}

}






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
request_database_new2 <- function(taxids, basemap) {
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
  headers = c(
    `Content-Type` = "application/x-www-form-urlencoded"
  )

  DATA <- NULL
  i <- 1
  while (i <= length(taxids)) {
    #requests are made 100 by 100
    taxids_sub <- taxids[i:(i + 99)]
    taxids_sub <- taxids_sub[!is.na(taxids_sub)]
    # adding the URL separator between the taxids
    taxids_sub <- paste(taxids_sub, collapse = " ")

    request <- paste("q=taxid:(", taxids_sub,
                 ")&rows=198&fl=taxid,ascend,lat,lon,zoom",
                 sep = "", collapse = "")

    # doing the request :

    res <- httr::POST(url = "https://lifemap-ncbi.univ-lyon1.fr/solr/taxo/query", httr::add_headers(.headers=headers), body = request)
    content_answer <- content(res)
    if(isTRUE(good_request(content_answer))){
      data_fetched <- formating(content_answer)
      DATA <- rbind(DATA, data_fetched)
    }
    # return(DATA)
    i <- i + 100
  # }
  #
  # # formating the dataframe
  # if (!(is.null(DATA))) {
  #   for (j in 1:(ncol(DATA)-1)) DATA[, j] <- unlist(DATA[, j])
  #   class(DATA$lon) <- "numeric"
  #   class(DATA$lat) <- "numeric"
  #   class(DATA$zoom) <- "numeric"
  #   class(DATA$taxid) <- "character"
  }
  # print(DATA)
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

  not_found <- c()
  if (nrow(df_distinct) != nrow(DATA_REQUEST)) {
    not_found <- c()
    for (id in df_distinct$taxid) {
      if (!(id %in% DATA_REQUEST$taxid)) {
        not_found <- append(not_found, id)
      }
    }
  }

  # if the root was in the requested taxids, add it now so as not to lose any information
  if (0 %in% df$taxid) {
    not_found <- not_found[not_found != 0]
    LUCA <- data.frame(taxid = "0", lon = 0, lat = -4.226497, sci_name = "Luca", zoom = 5)
    DATA_REQUEST <- dplyr::bind_rows(DATA_REQUEST, LUCA)
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

  INFOS_DATA$ancestor <- sapply(INFOS_DATA$ascend, "[[", 1)

  if (!(0 %in% df$taxid)) {
    LUCA <- data.frame(taxid="0",lon=0, lat=-4.226497,sci_name="Luca",zoom=5, type="ancestor")
  }

  FINAL_DATA <- dplyr::bind_rows(INFOS_DATA, LUCA)

  lm_obj <- list(df = FINAL_DATA, basemap = basemap)
  class(lm_obj) <- c("lifemap_obj", "list")

  return(lm_obj)
}










require(httr)

headers = c(
  `Content-Type` = "application/x-www-form-urlencoded"
)

data = upload_file("../../Bureau/curlfile.dd")
res2 <- httr::POST(url = "https://lifemap-ncbi.univ-lyon1.fr/solr/taxo/query", httr::add_headers(.headers=headers), body = data)
content_answer <- content(res2)

data = "q=taxid:(0 2 6 7 9 10 11 13 14 16 17 18 19 20 21 22 23 24 25 29 31 32 33 34 35 38 39 40 41 42 43 44 45 47 48 49 50 51 52 53 54 55 56 57 59 61 62 63 64 65 68 69 71 72 75 78 81 82 83 84 85 86 87 88 89 93 94 96 99 100 101 102 103 104 105 106 107 108 109 110 112 113 114 117 118 119 120 122 123 124 125 126 127 128 133 134 136 137 138 139 140 141 142 143 144 145 146 147 148 149 150 151 152 154 156 157 158 159 160 161 162 163 164 165 166 167 168 170 171 172 173 174 176 178 179 180 183 185 187 188 189 190 191 192 193 194 195 196 197 198 199 200 201 202 203 204 205 206 207 209 210 211 212 213 214 215 216 217 218 222 223 226 227 228 232 234 235 236 237 238 239 242 244 245 246 247 249 250 251 252 253 254 255 256 258 259 262 263)&rows=198&fl=taxid,ascend,lat,lon,zoom"

res3 <- httr::POST(url = "https://lifemap-ncbi.univ-lyon1.fr/solr/taxo/query", httr::add_headers(.headers=headers), body = data)

yey4 <- formating(string = content(res3))
