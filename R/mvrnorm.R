#' Multivariate normal random vector generator
#'
#' Generates vectors iid. from the multivariate normal distribution.
#'
#' @param n number of realizations
#' @param mu mean (\eqn{\mu})
#' @param Sigma positive definite covariance matrix (\eqn{\Sigma})
#' @return Returns a matrix of \code{n} stacked vectors, each an iid. realization of \eqn{N(\mu,\Sigma)}.
#' @export
mvrnorm = function(n,mu=0,Sigma,reps=1) {

    p = length(mu)
    Sigma.dim = dim(Sigma)
    stopifnot(isSymmetric.matrix(Sigma),p == dim(Sigma)[1],reps >= 1)

    # Calculate the eigen-decomposition of Sigma andSigma^(1/2),
    eigen.object = eigen(Sigma, symmetric = T)
    Sigma.sqrt = t(eigen.object$vectors) %*% diag(eigen.object$values^0.5) %*%
        eigen.object$vectors

    # Returns a matrix or list of matrices that is/are row-wose iid
    # N_p(mu,Sigma) of size n x p.
    if (reps == 1) {
        # Generate an n-by-p random standard normal matrix and
        # n-by-p matrix that is row-wise iid N_p(mu,Sigma)
        Z = matrix(rnorm(n*p), nrow = n, ncol = p)
        N = rep(1, n) %*% t(mu) + Z %*% Sigma.sqrt
        return (N)
    } else {
        # Do the above for reps repeats and return a list of the
        # results.
        N.list <- as.list(rep(0,reps))
        for (i in 1:reps) {
            Z = matrix(rnorm(n*p), nrow = n, ncol = p)
            N.list[[i]] = rep(1, n) %*% t(mu) + Z %*% Sigma.sqrt
        }
        return(N.list)
    }
}
