#' create a matrix filled with 0 and 1
#'
#' @description
#' 1 if the ancestor appears in the ascendants of a descendant, 0 either
#'
#' @param df a dataframe from a lifemap object
#' @return
#'
#' @example
#' load("data/taxids_10000.RData")
#' LM <- build_Lifemap(taxids_10000,"fr")
#' create_matrix(LM$df)
create_matrix <- function(df) {
  all_ancestors <- unique(unlist(df[df$type=="requested",]$ascend))
  tax_request <- as.vector(df[df$type=="requested",]$taxid)
  only_leaves <- as.list(setdiff(tax_request,all_ancestors))
  print("step 1")

  # create a list of dataframes : one for each leaf, containing all it's ancestors
  info_tmp <- lapply(only_leaves, FUN = function(x) {
    df_tmp <- data.frame(unlist(df[df$taxid==x,"ascend"]),1)
    colnames(df_tmp) <- c("ancestors",x)
    df_tmp
  })
  print("step 2")
  # merging all the dataframes together
  final_df <- reduce(info_tmp, full_join, by = "ancestors") %>% replace(., is.na(.), 0)
  return(final_df)
}
