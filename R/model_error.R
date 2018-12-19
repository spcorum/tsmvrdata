#' Model error
#'
#' Given a positive definite covariance matrix \eqn{\Sigma}, this
#' function calculates the model error between two matrices \eqn{X1}
#' and \eqn{X2} according to the formula
#' \deqn{Trace [ (U-W)' \Sigma' \Sigma (U-W) ]/(mn),}
#' where \eqn{m}, \eqn{n} are the dimensions of \eqn{U} (\eqn{W}).
#' If no covariance matrix is specified, the function calculates the
#' model error using the identity covariance matrix, which is
#' equivalent the normalized squared matrix error).
#'
#' @param U matrix (m-by-n)
#' @param W matrix (m-by-n)
#' @param Sigma positive definite covariance matrix (m-by-m)
#' @return The model error between \code{U} and \code{W} given
#' \code{Sigma}.
#'
# #' @note
# #' See also \code{\link[tsmvr]{squared_error}}.
#'
#' @references
#' \insertRef{MRCE}{tsmvrextras}
#'
#' @export
model_error <- function(U, W, Sigma = NULL) {
  stopifnot(all(dim(U) == dim(W)))
  if (is.null(Sigma)) return(norm(U - W, type = "f") / dim(U)[1] / dim(W)[2])
  return(norm(expm::sqrtm(Sigma) %*% (U - W), type = "f") / dim(U)[1] / dim(W)[2])
}
