#' Construct covariance and precision matrices
#'
#' Constructs a covariance matrix and its associated precision matrix
#' of the following types: first-order autoregressive [AR(1)],
#' fractional gaussian noise [FGN], or scale free network [SFN].
#'
#' @param q dimension of covariance matrix (positive integer)
#' @param type type of covariance matrix (string: 'AR1', 'FGN', or 'SFN')
#' @param rho autoregression parameter for AR(1) covariance matrix (0 < \code{rho_err} < 1)
#' @param h Hurst parameter for FGN covariance matrix (0 < \code{H} < 1)
#' @param n_edge Barabasi algorithm number of edges per step for SFN covariance matrix (positive integer)
#' @param shift eigenvalue shift parameter for SFN covariance matrix (\code{shift} > 0) (ensures matrix is PSD) )
#' @param power scaling power for SFN covariance matrix (positive numeric)
#' @param zero_appeal Barabasi algorithm baseline attractiveness for SFN covariance matrix (positive numeric)
#' @param g number of hub nodes for HUB graph precision matrix (positive integer-valued numeric less than q)
#' @param diag_val values of diagonal entries HUB graph precision matrix (non-negative numeric)
#' @param edge_val values of HUB graph network edges
#' @references
#' \insertRef{MRCE}{tsmvr}
#' \insertRef{chen2016high}{tsmvr}
#'
#' @return A list of two matrices, the \code{covariance} matrix and the
#' \code{precision} matrix.
# #' @export
covariance_matrix <- function(q, type = "AR1", rho = 0.7, h = 0.9,
                              n_edge = 1, shift = 1,  power = 1, zero_appeal = 1,
                              g = 1, diag_val = 1, edge_val = 0.3) {
  stopifnot(
    q %% 1 == 0, q > 0, rho >= 0, rho < 1, h >= 0, h < 1,
    power > 0, zero_appeal > 0, n_edge > 0, shift >= 0,
    g %% 1 == 0, g > 0, g <= q, diag_val > 0, edge_val >=  0
  )

  if (type == "AR1") {
    covariance = covar_ar1(q, rho)
    precision = zapsmall(chol2inv(chol(covariance)))
  }
  else if (type == "FGN") {
    covariance = covar_fgn(q, h)
    precision = zapsmall(chol2inv(chol(covariance)))
  }
  else if (type == "SFN") {
    precision = precision_sfn(
      q, power = power, zero_appeal = zero_appeal,
      n_edge = n_edge, shift = shift
    )
    covariance = zapsmall(chol2inv(chol(precision)))
  }
  else if (type == "HUB") {
    precision = precision_hub(q, g, diag_val, edge_val)
    covariance = zapsmall(chol2inv(chol(precision)))
  }
  return(list(covariance = covariance, precision = precision))
}
