library(dplyr)

#' Create a df with informations about ancestry (father and son nodes)
#'
#' @param df a dataframe
#'
#' @return a dataframe
#' @export
#'
#' @importFrom dplyr bind_rows
#'
#' @examples
get_full_ancestry <- function(df){
  ancestors <- c()
  df_ancestry <- data.frame("son"=numeric(0), "father"=numeric(0))

  for (id in 1:nrow(df[df$type == "r",])){
    ancestry <- unlist(df[id,"ascend"])
    # print(class(df[id,"taxid"]))
    # print(class(ancestry[1]))
    son_father <- data.frame("son"=as.integer(df[id,"taxid"]),"father"=ancestry[1])
    df_ancestry <- dplyr::bind_rows(df_ancestry, son_father)
    for (x in 2:length(ancestry)){
      son_father <- data.frame("son"=ancestry[x-1],"father"=ancestry[x])
      df_ancestry <- dplyr::bind_rows(df_ancestry, son_father)
      if (!(ancestry[x] %in% ancestors)){
        ancestors <- append(ancestors, ancestry[x])
      } else {
        break
      }
    }
  }
  final_df <- merge(df, df_ancestry, by.x="taxid", by.y="son")
  return(final_df)
}
