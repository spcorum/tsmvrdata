#' Calculate error curve
#'
#' Calculates the error curve for a list of matrices
#'
#' Given a list of matrices representing iterates of some optimization
#' algorithm and an optional "ground truth" matrix \code{Star}, this
#' function calculates the "error curve", which is the Frobenius norm
#' between each iterate and (1) the final iterate (if \code{Star} is
#' not specified) or (2) the ground truth (if \code{Star} is specified).
#'
#' @param Hat.list a list of matrices (each dimension m-by-n)
#' representing the iterates of an optimization algorithm
#' @param Star matrix (m-by-n) representing the ground truth (optional)
#'
#' @return An error curve (numeric vector).
#'
#' @note
#' See also \code{\link[tsmvr]{tsmvr_solve}} and
#' \code{\link{plot_error_curve}}.
#'
#' @export
error_curve <- function(Hat.list, Star = NULL) {

  stopifnot(
    is.list(Hat.list), is.matrix(Star) || is.null(Star),
    length(Hat.list) > 0, all(sapply(Hat.list, is.matrix)),
    length(unique(lapply(Hat.list, dim))) == 1,
    is.null(Star) || dim(Hat.list[[1]]) == dim(Star)
  )

  K <- length(Hat.list)
  error_curve <- rep(0, K)
  if (is.null(Star)) Star <- Hat.list[[K]]
  for (k in 1:K) error_curve[k] <- norm(Hat.list[[k]] - Star, "F")

  #stopifnot( expect_true(all(error_curve(Hat.list,Star) >= 0)) )

  return(error_curve)
}
