#' Create a dataframe for the ancestry
#'
#' @description
#' Create a dataframe with couples of taxids, each taxid is associated with each
#' one of it's ancestors, values are also associated with the taxids
#'
#'
#' @param df a dataframe containing taxids and values
#' @param cols the columns that contains the values
#'
#' @return a dataframe
#' @export
#'
#' @examples create_matrix(LM$df, c"GC., "Genes")
create_matrix <- function(df, cols){
  a <- sapply(1:nrow(df), function(x,y){
    cbind(y$taxid[x], c(y$taxid[x],y$ascend[x][[1]]))
  }, y = df)
  a <- a[-length(a)]
  B <- do.call(rbind, a)

  new_df <- as.data.frame(B)
  colnames(new_df) <- c("descendant", "ancestor")
  if (!is.null(cols)) {
    for (var in cols) {
      new_df <- full_join(new_df, df[,c("taxid", var)], by = join_by(descendant == taxid))
    }

  }
  return (new_df)
}

#' Infer values to nodes
#'
#' @param M the dataframe returned by create_matrix
#' @param FUN the function to be applied when passing the info
#' @param value the column to which the function apply
#'
#' @return
#' @export
#'
#' @examples
pass_infos <- function(M, FUN, value){
  inferred_values <- tapply(M[[value]], M$ancestor, function(x){
    x <- x[!is.na(x)]
    FUN(x)})
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
create_matrix_discret <- function(df) {
  all_tax <- df$taxid
  H <- apply(df[df$type == "requested", ], 1, function(x, y) y %in% c(x$taxid,x$ascend), y = all_tax)
  return(H)
}

#' pass the information to nodes
#'
#' @param M the boolean matrix
#' @param values a vector of the values
#' @param tax a vector of all the taxids
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
    count <- merge(df_uniques, count, by = "val", all = TRUE)
    print(count)
    count[is.na(count)] <- 0
    paste(count$Freq, sep = "", collapse = ",")
  })
  # print(inferred_values)
  new_vals <- str_split_fixed(inferred_values, ",", 4)
  df_values <- data.frame(tax,new_vals)
  colnames(df_values) <- c("tax",unique_values)
  for (value in unique_values) {
    class(df_values[[value]]) <- "numeric"
  }
  return(df_values)

}


