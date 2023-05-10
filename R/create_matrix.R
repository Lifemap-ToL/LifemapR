#' Create a dataframe for the ancestry
#'
#' @description
#' Create a dataframe with couples of taxids, each taxid is associated with each
#' one of it's ancestors, values are also associated with the taxids
#'
#' @param df a dataframe containing taxids and values
#' @param cols the columns that contains the values
#'
#' @return a dataframe
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
      new_df <- dplyr::full_join(new_df, df[,c("taxid", var)], by = dplyr::join_by(descendant == taxid))
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
#' @return an array of values
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


#' pass discret informations to nodes
#'
#' @param M the boolean matrix
#' @param value the variable concerned
#'
#' @return a dataframe
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



