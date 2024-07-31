#' Create a newick for the given dataset.
#'
#' @param df A dataframe either enriched with by the build_Lifemap function or containing at least two columns :
#' - "taxid" containing NCBI format taxids
#' - "ancestor" containing for each taxid, it's direct ancestor in the phylogeny
#'
#' @return A character string.
#' @export
#'
#' @importFrom stats complete.cases
make_newick <- function(df) {
  # df must contain, a minima, a column "taxid" and a column "ancestor"
  mat <- cbind(df$ancestor, df$taxid)

  # remove NA (if any)
  mat <- mat[stats::complete.cases(mat), ]

  # put the root edges as first rows
  whereroot <- which(is.na(match(mat[, 1], mat[, 2])))
  mat <- rbind(mat[whereroot, ], mat[-whereroot, ])
  i <- length(whereroot) + 1
  while (i < nrow(mat)) {
    while(!(mat[i, 1] %in% mat[1:(i - 1), 2])) {
      mat <- rbind(mat[-i, ], mat[i, ])
    }
    i <- i+1
  }

  ## Put secial characters ("-") before and after each name
  ## for preventing errors with gsub afterwards.
  matok <- t(apply(mat, 1, function(x) paste("-", x, "-", sep = "")))
  nodes <- unique(matok[, 1])
  nwk <- paste(matok[1, 2], ";", sep = "")
  for (n in nodes[-1]) {
    desc <- paste("(", paste(matok[which(matok[, 1]%in% n), 2], collapse = ","),")", n, sep = "")
    nwk <- gsub(n, desc, nwk)
  }
  # remove the special character
  NWK <- gsub("-", "", nwk)
  NWK
}
