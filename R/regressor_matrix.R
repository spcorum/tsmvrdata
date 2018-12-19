#' Sparse regressor matrix
#'
#' Creates a random realization of a sparse multivariate regressor
#' matrix.
#'
#' @param p number of regression features (positive integer)
#' @param q number of regression responses (positive integer)
#' @param b1 a Bernoulli parameter controlling the sparsity of the regressor matrix (0 <= \code{s1} <= 1)
#' @param b2 a second Bernoulli parameter controlling the sparsity of the regressor matrix (0 <= \code{s2} <= 1)
#'
#' @return Returns a random sparse regressor matrix of dimension
#' \code{p} x \code{q} with expected sparsity \code{b1} \eqn{x} \code{b2}.
#'
#' @note The regressor matrix is constructed such that \code{(1-s2)p}
#' predictors are expected to be irrelevant for \code{q} responses,
#' and each predictor is expected to be relevant for \code{(s1 q)} of
#' the response variables.
#'
#' See also \code{\link{tsmvrExtras.rbern}}.
#'
#' @references
#' \insertRef{MRCE}{tsmvrExtras}
#'
#' @export
regressor_matrix <- function(p, q, b1 = sqrt(0.1), b2 = sqrt(0.1)) {
  W <- matrix(rnorm(p * q), nrow = p, ncol = q)
  K <- matrix(tsmvr.rbern(p * q, b1), nrow = p, ncol = q)
  Q <- matrix(0, nrow = p, ncol = q)
  for (l in 1:p) Q[l, ] <- rep(tsmvr.rbern(1, b2), q)
  return(W * K * Q)
}
