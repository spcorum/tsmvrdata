#' Make a synthetic dataset
#'
#' Creates a random synthetic dataset for sparse multivariate
#' regression accoring to the model:
#' \deqn{Y = BX + E,}
#' where \eqn{X} is the design matrix, \eqn{B} is the regressor
#' matrix, \eqn{Y} is the response matrix, and \eqn{E} is the matrix
#' error term.
#'
#' @param n number of observations (positive integer)
#' @param p number of regressor features (positive integer)
#' @param q number of responses (postive integer)
#' @param b1 Beornoulli parameter for controlling regressor matrix sparsity (positive integer)
#' @param b2 another Beornoulli parameter for controlling regressor matrix sparsity (positive integer)
#' @param sigma scale of error term (positive numeric)
#' @param rho_x autoregression parameter for design matrix covariance matrix (0 < \code{rho} < 1)
#' @param type type of covariance matrix (string: 'AR1', 'FGN', or 'SFN')
#' @param vary_x whether or not to vary the design matrix (bool)
#' @param rho_err autoregression parameter for AR(1) covariance matrix (0 < \code{rho} < 1)
#' @param h Hurst parameter for FGN covariance matrix (0 < \code{h} < 1)
#' @param power scaling power for SFN covariance matrix (positive numeric)
#' @param zero_appeal Barabasi algorithm baseline attractiveness for SFN covariance matrix (positive numeric)
#' @param n_edge Barabasi algorithm number of edges per step for SFN covariance matrix (positive integer)
#' @param min_ev minimum eigenvalue of SFN covariance matrix (\code{min_ev} > 0) (ensures matrix is PSD) )
#' @param reps number of randomly drawn datasets to return (positive integer)
#' @param seed random seed for reproducibility (positive integer)
#'
#' @return Returns a list of length \code{reps}. Each entry is itself
#' a comprising a synthetic sparse multivariate dataset: a regressor
#' matrix \code{B}, a design matrix \code{X}, and a response matrix
#' \code{Y}. \code{B} has an expected sparsity of \code{b1} \code{x}
#' \code{b2}.
#'
#' See also \code{\link{tsmvrextras.regressor_matrix}},
#' \code{\link{tsmvrextras.covariance_matrix}}, and
#' \code{\link[tsmvr]{tsmvr_solve}}.
#'
#' @references
#' \insertRef{MRCE}{tsmvrExtras}
#'
#' @export
make_data <- function(n, p, q, b1 = sqrt(0.1), b2 = sqrt(0.1), sigma, rho_x = 0.6,
                      type = "AR1", vary_x = T, rho_err = 0.7, h = 0.9, a = 1, b = 1, n_edge = 1,
                      min_ev = 0.18, reps = 1, seed = NULL) {
  stopifnot(reps >= 1, type == "AR1" || type == "FGN" || type == "SFN")
  set.seed(seed)

  # --------------------------------------------------------------------
  # The code creates a rep datasets according to the multivariate model:
  #                       Y = X*B + E
  # --------------------------------------------------------------------

  # Calculate the auxillary and error covariance matrices --------------
  Sigma_x <- covariance_matrix(
    p, rho_x = rho_x, type = "AR1", reps = reps
  )
  Sigma_err <- covariance_matrix(
    q,
    sigma = sigma, rho_err = rho_err, type = type, reps = reps
  )

  # Calculate a random (list of random draws of the) dataset -----------
  X.list <- mvrnorm(n, rep(0, p), Sigma_x$covariance, reps)
  E.list <- mvrnorm(n, rep(0, q), Sigma_err$covariance, reps)
  # data.list <- as.list(rep(0, reps))
  data.list <- as.list(rep(0, reps), regressor_matrix(p, q, b1, b2),
                       how = 'replace')

  for (i in 1:reps) {
    # B <- regressor_matrix(p, q, b1, b2)
    if (vary_x) {
      Y <- X.list[[i]] %*% B + sigma^2 * E.list[[i]]
      data.list[[i]]$X <- X.list[[i]]
    }
    else {
      Y <- X.list[[1]] %*% B + sigma^2 * E.list[[i]]
      data.list[[i]]$X <- X.list[[1]]
    }
    data.list[[i]]$Y <- Y
    data.list[[i]]$Y <- Y
    data.list[[i]]$B_star <- B
    data.list[[i]]$Omega_star <- Sigma_err$precision
    data.list[[i]]$Sigma_err <- Sigma_err$covariance
    data.list[[i]]$Sigma_x <- Sigma_x
  }

  return(data.list)
}
