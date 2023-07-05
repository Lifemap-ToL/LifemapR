#' Create a dataframe for the ancestry.
#'
#' @description
#' Create a dataframe with couples of taxids, each taxid and their corresponding values are associated with each
#' one of the taxid's ancestors.
#'
#' @param df A dataframe containing taxids and values.
#' @param cols The columns that contains the values which needs to be inferred.
#'
#' @return A dataframe.
#' @export
#' @importFrom dplyr full_join join_by
#'
#' @examples
#' data(LM_eukaryotes)
#'
#' create_matrix(LM_eukaryotes$df, c("GC.", "Genes"))
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
      new_df <- dplyr::full_join(new_df, df[,c("taxid", var)], by = dplyr::join_by("descendant" == "taxid"))
    }

  }
  return (new_df)
}

#' Infer numerical values to nodes.
#'
#' @param M The dataframe returned by create_matrix.
#' @param FUN The function to be applied when inferring the values.
#' @param value The column name to which the function applies.
#'
#' @return An array of values.
#' @export
#'
#' @examples
#' data(LM_eukaryotes)
#'
#' infos <- create_matrix(LM_eukaryotes$df, c("GC.", "Genes"))
#'
#' inferred_values <- pass_infos(M = infos, FUN = mean, value = "GC.")
pass_infos <- function(M, FUN, value){
  inferred_values <- tapply(M[[value]], M$ancestor, function(x){
    x <- x[!is.na(x)]
    FUN(x)})
}


#' Pass discret informations to nodes
#'
#' @param M The dataframe returned by create_matrix.
#' @param value The column name to which the function applies.
#'
#' @return A dataframe.
#' @export
#' @importFrom dplyr bind_rows
#'
#' @examples
#' data(LM_eukaryotes)
#'
#' infos <- create_matrix(LM_eukaryotes$df, "Status")
#'
#' inferred_values <- pass_infos_discret(M = infos, value = "Status")
pass_infos_discret <- function(M, value) {
  tabled <- tapply(M[[value]], M$ancestor, function(x){
    x <- x[!is.na(x)]
    table(x)})
  bind_values <- as.data.frame(dplyr::bind_rows(tabled))
  bind_values[is.na(bind_values)] <- 0
  bind_values$taxid <- names(tabled)
  return(bind_values)
}



