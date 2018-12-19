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
make_data = function(n,p,q,b1=sqrt(0.1),b2=sqrt(0.1),sigma=1,rho_x=0.6,
                     type='AR1',rho_err=0.7,h=0.9,a=1,b=1,n_edge=1,
                     min_ev=0.18,reps=1,seed=NULL) {

    stopifnot(reps>=1, type=='AR1' || type == 'FGN' || type=='SFN')

    # ############################################################### #
    # The code creates a dataset according to the multivariate model: #
    #                                                                 #
    #                    Y = X*B + E                                  #
    #                                                                 #
    # ############################################################### #

    # Calculate the covariance and precision matrices.
    set.seed(seed)
    # Sigma.X = covmat_ar1(p, rho_x)
    # if (type=='AR1') Sigma.E = covmat_ar1(q, rho_err)
    # else if (type=='FGN') Sigma.E = covmat_fgn(q, H)
    # else if (type=='SFN') {
    #     Omega = covmat_sfn(q,a=a,b=b,n_edge=n_edge,
    #                               min_ev=min_ev)
    #     Sigma.E = chol2inv(chol(Omega))
    # }
    # if (type=='AR1'||type=='FGN')
    #     Omega = zapsmall(chol2inv(chol(Sigma.E)))
    sigma.list = covariance_matrix(q, sigma, rho_x, type, rho_err, h, a,
                                   b, n_edge, min_ev=0.18)

    # if (reps==1) {  # Single simulations.
    #     X = mvrnorm(n,rep(0,p),Sigma.X)
    #     E = mvrnorm(n,rep(0,q),Sigma.E)
    #     B = regressor_matrix(p,q,b1,b2)
    #     Y = X%*%B+sigma^2*E
    #     return(list(X=X, Y=Y, B.star=B, Omega.star=Omega,
    #                 Sigma.E=Sigma.E, Sigma.X=Sigma.X))
    # } else {  # Multiple simulations.

    X.list = mvrnorm(n, rep(0, p), Sigma.X, reps)
    E.list = mvrnorm(n, rep(0, q), sigma.list$covariance, reps)
    data.list = as.list(rep(0, reps))
    for (i in 1:reps) {
        B = regressor_matrix(p, q, b1, b2)
        Y = X.list[[i]] %*% B + sigma^2 * E.list[[i]]
        data.list[[i]]$X = X.list[[i]]
        data.list[[i]]$Y = Y
        data.list[[i]]$B.star = B
        data.list[[i]]$Omega.star = sigma.list$precision
        data.list[[i]]$Sigma.E = sigma.list$covariance
        data.list[[i]]$Sigma.X = Sigma.X
    }
    return(data.list)

    #   }
}
