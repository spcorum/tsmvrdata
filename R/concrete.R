#' Measurements of concrete slump as a function of concrete attributes
#'
#' A dataset containing 103 observations of concrete slump and concrete material
#' attribute measurements. Compared to the data at the source url,
#' all data columns have been cleaned, centered, and normalized for
#' regression.
#'
#' Citation: Yeh, I-Cheng, "Modeling slump flow of concrete using second-order regressions and artificial neural networks," Cement and Concrete Composites, Vol.29, No. 6, 474-480, 2007.
#'
#' @format A data frame with 103 rows and 11 variables:
#' \describe{
#'   \item{id number (metadata)}
#'   \item{Cement (predictor)}
#'   \item{Slag (predictor)}
#'   \item{Fly.ash (predictor)}
#'   \item{Water (predictor)}
#'   \item{SD (predictor)}
#'   \item{Course.Aggr (predictor)}
#'   \item{Fine.Aggr (predictor)}
#'   \item{SLUMP.cm (response)}
#'   \item{FLOW.cm (response)}
#'   \item{Compressive.Strength..28.day..Mpa (response)}
#' }
#'
#' @source \url{https://archive.ics.uci.edu/ml/datasets/Concrete+Slump+Test} "Concrete Slump Dataset at UC Irvince Machine Learning Data Repository"
