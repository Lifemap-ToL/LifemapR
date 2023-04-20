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
#' @examples
#' data("taxids_10000")
#' LM <- build_Lifemap(taxids_10000,"fr")
#' create_matrix(LM$df)
create_matrix <- function(df) {
  all_ancestors <- unique(unlist(df[df$type == "requested", ]$ascend))
  H <- apply(df[df$type == "requested", ], 1, function(x, y) y %in% x$ascend, y = all_ancestors)
  return(H)
}


#' pass the information to nodes
#'
#' @param M the boolean matrix
#' @param func the function to be applied
#' @param value a vector of the values
#'
#' @return a dataframe
#' @export
pass_infos <- function(M, values, ancestors, my_func) {
  my_func <- match.fun(my_func)
  inferred_values <- apply(X = M, MARGIN = 1, FUN = function(x){
    trues <- which(x == TRUE)
    trues <- as.vector(trues)
    val <- values[trues]
    val <- val[!is.na(val)]
    my_func(val)
  })
  data.frame(ancestors = ancestors,
             value = inferred_values)
}




#' create a boolean matrix
#'
#' @description
#' TRUE if the ancestor appears in the ascendants of a descendant, FALSE either
#'
#' @param df a dataframe from a lifemap object
#' @export
#' @importFrom stringr str_split_fixed
#'
#' @return a dataframe filled with TRUE or FALSE
#'
#' @examples data("taxids_10000")
#' LM <- build_Lifemap(taxids_10000,"fr")
#' create_matrix_discret(LM$df)
create_matrix_discret <- function(df) {
  all_tax <- df$taxid
  H <- apply(df[df$type == "requested", ], 1, function(x, y) y %in% c(x$taxid,x$ascend), y = all_tax)
  return(H)
}

#' pass the information to nodes
#'
#' @param M the boolean matrix
#' @param func the function to be applied
#' @param value a vector of the values
#'
#' @return a dataframe
#' @export
pass_infos_discret <- function(M, values, tax) {
  # my_func <- match.fun(my_func)
  # get all the values possible for the variable (eg. for Status there is Chromosome, Scaffold ...)
  unique_values <- unique(values)
  df_uniques <- data.frame(val = unique_values)

  # inferring values to the nodes
  inferred_values <- apply(X = M, MARGIN = 1, FUN = function(x){
    trues <- which(x == TRUE)
    trues <- as.vector(trues)
    val <- values[trues]
    val <- val[!is.na(val)]
    count <- data.frame(table(val))
    count <- merge(df_uniques, count, by="val", all=TRUE)
    count[is.na(count)] <- 0
    paste(count$Freq, sep="", collapse = ",")
  })
  new_vals <- str_split_fixed(inferred_values, ",", 4)
  df_values <- data.frame(tax,new_vals)
  colnames(df_values) <- c("tax",unique_values)
  for (value in unique_values) {
    class(df_values[[value]]) <- "numeric"
  }
  return(df_values)

}


