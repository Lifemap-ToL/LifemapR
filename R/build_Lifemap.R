#' A function to construct a LifemapR object, usable by the other functions of the package.
#'
#' @param df A dataframe containing at least one column named "taxid" that contains NCBI Taxonomy Identifiers (taxid).
#' The dataframe can contain any number of additional columns defining traits/characters/values associated to
#' each taxid.
#' @param basemap Deprecated argument.
#' @param verbose If TRUE (the default), the function will print detailed information to the console.
#' If FALSE, it will run silently.
#'
#' @return A list of class lifemap_obj containing:
#' - df : a dataframe containing at least for each taxid :
#'   - The x coordinate (lon)
#'   - The y coordonate (lat)
#'   - The scientific name (sci_name)
#'   - The zoom level at which the taxa is visible (zoom)
#'   - A list of its ascendants (ascend)
#'   - Its type ("requested" or "ancestor")
#'   - Its direct ancestor
#'   - Its type (type), i.e. whether the taxid was
#' requested by the user ("requested") or if it is the anecestor of a requested taxid ("ancestor")
#' - basemap : the basemap used to get taxa's details
#'
#' @importFrom jsonlite fromJSON
#' @importFrom dplyr bind_rows distinct
#' @importFrom rlang .data
#' @importFrom RCurl url.exists
#' @importFrom fastmatch fmatch
#'
#' @export
#' @examples
#' data(eukaryotes_80)
#' \dontrun{
#' # make sure you have a good internet connection to load these very large files
#' LM <- build_Lifemap(eukaryotes_80)
#' }
build_Lifemap <- function(df, basemap = NULL, verbose = TRUE) {
  if (!is.null(basemap)) {
    warning("The basemap argument is now deprecated.")
  }
  basemap_url <- "https://lifemap-back.univ-lyon1.fr/data/lmdata.Rdata"

  if (is.null(df$taxid)) {
    stop('The dataframe must at least contain a "taxid" column')
  }
  # create a new "environment" to store the full data
  if (!exists("lifemap_basemap_envir", where = .GlobalEnv)) {
    lifemap_basemap_envir <- new.env()
  }

  y <- tryCatch(
    {
      load(url(basemap_url), envir = lifemap_basemap_envir)
    },
    warning = function(w) {
      print(w)
      message("The Lifemap server or some remote lifemap files cannot be reached. Please try again later.")
      return(NA)
    },
    error = function(e) {
      print(e)
      message("The Lifemap server or some remote lifemap files cannot be reached. Please try again later.")
      return(NA)
    }
  )

  if (!is.na(y)) {
    # download full data for chosen basemap
    if (verbose) {
      cat("Downloading basemap coordinates...\n")
    }
    load(url(basemap_url), envir = lifemap_basemap_envir)

    # add LUCA
    LUCA <- data.frame("taxid" = "0", "lon" = 0, "lat" = -4.226497, "sci_name" = "Luca", "zoom" = 5)
    lifemap_basemap_envir$DF <- dplyr::bind_rows(lifemap_basemap_envir$DF, LUCA)

    # get info for unique taxids (then we work with df_distinct, not df anymore)
    df_distinct <- dplyr::distinct(df, .data$taxid, .keep_all = TRUE)
    if (nrow(df_distinct) != nrow(df)) {
      warning(sprintf("%s duplicated TaxIDs were removed \n", nrow(df) - nrow(df_distinct)))
    }

    # get data
    if (verbose) {
      cat("Getting info for requested taxids...\n")
    }

    # get index of requested taxids
    indexes <- fastmatch::fmatch(df_distinct$taxid, lifemap_basemap_envir$DF$taxid)
    if (sum(is.na(indexes)) > 0) {
      warning(sprintf(
        "%s TaxID(s) could not be found: %s \n",
        sum(is.na(indexes)),
        paste(df_distinct$taxid[is.na(indexes)], sep = ",")
      ))
    }

    # create new df with only existing taxids
    df_exists <- df_distinct[!is.na(indexes), ]
    DATA0 <- lifemap_basemap_envir$DF[indexes[!is.na(indexes)], ]

    # get ancestors
    unique_ancestors <- unique(unlist(DATA0$ascend))
    real_ancestors <- setdiff(unique_ancestors, df_exists$taxid)
    ANCESTORS <- lifemap_basemap_envir$DF[fastmatch::fmatch(real_ancestors, lifemap_basemap_envir$DF$taxid), ]

    # add type
    DATA0$type <- "requested"
    ANCESTORS$type <- "ancestor"
    # bind all
    DATA1 <- dplyr::bind_rows(DATA0, ANCESTORS)

    # merge
    DATA2 <- merge(DATA1, df_exists, by = "taxid", all = TRUE)

    # replace the column 'ascend' by simply the direct ancestor
    DATA2$ancestor <- unlist(lapply(DATA2$ascend, function(x) ifelse(!is.null(x), x[1], NA)))

    lm_obj <- list(df = DATA2, basemap = basemap)
    class(lm_obj) <- c("lifemap_obj", "list")

    return(lm_obj)
  } else {
    return(NA)
  }
}
