#' @title Cleaned Master Tree Species List from FIA
#'
#' @description A cleaned dataset containing tree species recorded by the
#' Forest Inventory and Analysis (FIA) program of the U.S. Forest Service.
#' This dataset is used in the examples and README of the \code{wcvpmatch}
#' package. The data was downloaded in November 2022 from the official
#' webpage of the Forest Inventory and Analysis National Program, available
#' at the following \href{https://www.fia.fs.usda.gov/library/field-guides-methods-proc/index.php}{link},
#' and was originally used during the development of the \code{treemendous}
#' package. For \code{wcvpmatch}, the variable names have been standardized
#' to \code{Orig.Genus} and \code{Orig.Species}.
#'
#' @format A data frame with `r nrow(fia)` rows and `r ncol(fia)` variables:
#' \describe{
#'   \item{Orig.Genus}{Genus name of the species binomial}
#'   \item{Orig.Species}{Specific epithet of the species binomial}
#' }
"fia"
