

## generating the data from local files
# column.df <- read.delim("test/columns.txt")
# column.df$treatment <- factor(column.df$treatment, levels=c("Nitrate", "Drought", "ABA"))
#
# row.df <- read.delim("test/rows.txt")
# value.df <- read.delim("test/values.txt")
#
# row.df$numeric <- sample(c(1:10, NA), nrow(row.df), replace=T)
# row.df$norm <- rnorm(nrow(row.df), 1)
#
# ND = list(values=value.df, columns=column.df, rows=row.df)
# save(ND, file="data/ND.rda")

#' Test Nitrogen/Drought dataset
#'
#' Data from a meta-analsis of N and D projects in the SRA. L2FC data for a set of genes and libraries in a matrix. Includes values, and metadata for genes and libraries.
#'
#' @docType data
#'
#' @usage data(ND)
#'
#' @format An object of class \code{"cross"}; see \code{\link[qtl]{read.cross}}.
#'
#' @keywords datasets
#'
#'
#'
#' @examples
#' data(ND)
#'
#'
"ND"
