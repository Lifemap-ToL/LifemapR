#' create a boolean matrix
#'
#' @description
#' TRUE if the ancestor appears in the ascendants of a descendant, FALSE either
#'
#' @param df a dataframe from a lifemap object
#' @export
#'
#' @return a dataframe filled with TRUE or FALSE
#'
#' @examples load("data/taxids_10000.RData")
#' LM <- build_Lifemap(taxids_10000,"fr")
#' create_matrix(LM$df)
create_matrix <- function(df) {
  all_ancestors <- unique(unlist(df[df$type == "requested", ]$ascend))
  H <- apply(df[df$type == "requested", ], 1, function(x, y) y %in% x$ascend, y = all_ancestors)
  return(H)
}


