#' Model error
#'
#' Given a positive definite covariance matrix \eqn{\Sigma}, this
#' function calculates the model error between two matrices \eqn{X1}
#' and \eqn{X2} according to the formula
#' \deqn{Trace [ (X1-X2)' \Sigma' \Sigma (X1-X2) ]/(mn),}
#' where \eqn{m}, \eqn{n} are the dimensions of \eqn{X1} (\eqn{X2}).
#' If no covariance matrix is specified, the function calculates the
#' model error using the identity covariance matrix, which is
#' equivalent the normalized squared matrix error).
#'
#' @param X1 matrix (m-by-n)
#' @param X2 matrix (m-by-n)
#' @param Sigma positive definite covariance matrix (m-by-m)
#' @return The model error between \code{X1} and \code{X2} given
#' \code{Sigma}.
#'
#' @note
#' See also \code{\link{squared_error}}.
#'
#' @references
#' \insertRef{MRCE}{tsmvrExtras}
#'
#' @export
model_error <- function(X1, X2, Sigma = NULL) {
  stopifnot(all(dim(X1) == dim(X2)))
  if (is.null(Sigma)) return(squared_error(X1, X2))
  return(1 / (dim(X1)[1] * dim(X1)[2]) * tr(crossprod(X1 - X2) %*% Sigma))
}
