#' Construct covariance and precision matrices
#'
#' Constructs a covariance matrix and its associated precision matrix
#' of the following types: first-order autoregressive [AR(1)],
#' fractional gaussian noise [FGN], or scale free network [SFN].
#'
#' @param q dimension of covariance matrix (positive integer)
#' @param rho_x autoregression parameter for modeling the design matrix covariance (0 < \code{rho_x} < 1)
#' @param type type of covariance matrix (string: 'AR1', 'FGN', or 'SFN')
#' @param rho_err autoregression paramter for AR(1) covariance matrix (0 < \code{rho_err} < 1)
#' @param H Hurst parameter for FGN covariance matrix (0 < \code{H} < 1)
#' @param power scaling power for SFN covariance matrix (positive numeric)
#' @param zero_appeal Barabasi algorithm baseline attractiveness for SFN covariance matrix (positive numeric)
#' @param n_edge Barabasi algorithm number of edges per step for SFN covariance matrix (positive integer)
#' @param min_ev minimum eigenvalue of SFN covariance matrix (\code{min_ev} > 0) (ensures matrix is PSD) )
#' @references
#' \insertRef{MRCE}{tsmvrExtras}
#'
#' \insertRef{chen2016high}{tsmvr}
#' @return A list of two matrices, the \code{covariance} matrix and the
#' \code{precision} matrix.
#' @export
covariance_matrix = function(q, rho_x=0.6, type='AR1', rho_err=0.7, H=0.9,
                             power=1, zero_appeal=1, n_edge=1,
                             min_ev=0.18) {

    Sigma.X = covmat_ar1(p, rho_x)
    if (type=='AR1') {
        covariance = covmat_ar1(q, rho_err)
        precision = zapsmall(chol2inv(chol(covariance)))
    }
    else if (type=='FGN') {
        covariance = covmat_fgn(q, H)
        precision = zapsmall(chol2inv(chol(covariance)))
    }
    else if (type=='SFN') {
        precision = covmat_sfn(q,power=power,zero_appeal=zero_appeal,
                               n_edge=n_edge,min_ev=min_ev)
        covariance = zapsmall(chol2inv(chol(precision)))
    }
    return(list(covariance = covariance, precision = precision))
}
