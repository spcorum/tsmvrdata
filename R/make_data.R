#' Make a synthetic dataset
#'
#' Creates a random synthetic dataset for sparse multivariate
#' regression according to the model:
#' \deqn{Y = BX + E,}
#' where \eqn{X} is the design matrix, \eqn{B} is the regressor
#' matrix, \eqn{Y} is the response matrix, and \eqn{E} is the matrix
#' error term.
#'
#' @param n number of observations (positive integer)
#' @param p number of regressor features (positive integer)
#' @param q number of responses (positive integer)
#' @param b1 Bernoulli parameter for controlling regressor matrix sparsity (positive integer)
#' @param b2 another Bernoulli parameter for controlling regressor matrix sparsity (positive integer)
#' @param sigma scale of error term (positive numeric)
#' @param rho_x autoregression parameter for design matrix covariance matrix (0 < \code{rho} < 1)
#' @param type type of covariance matrix (string: 'AR1', 'FGN', or 'SFN')
#' @param rho_err autoregression parameter for AR(1) covariance matrix (0 < \code{rho} < 1)
#' @param h Hurst parameter for FGN covariance matrix (0 < \code{h} < 1)
#' @param n_edge Barabasi algorithm number of edges per step for SFN covariance matrix (positive integer)
#' @param shift eigenvalue shift parameter for SFN covariance matrix (\code{shift} > 0) (ensures matrix is PSD) )
#' @param power scaling power for SFN covariance matrix (positive numeric)
#' @param zero_appeal Barabasi algorithm baseline attractiveness for SFN covariance matrix (positive numeric)
#' @param g number of hub nodes for HUB graph precision matrix (positive integer-valued numeric less than q)
#' @param diag_val values of diagonal entries HUB graph precision matrix (non-negative numeric)
#' @param edge_val values of HUB graph network edges
#' @param reps number of randomly drawn datasets to return (positive integer)
#' @param seed seed for pseudo-random number generator
#'
#' @return Returns a list of length \code{reps}. Each entry is itself
#' a comprising a synthetic sparse multivariate dataset: a regressor
#' matrix \code{B}, a design matrix \code{X}, and a response matrix
#' \code{Y}. \code{B} has an expected sparsity of \code{b1} \code{x}
#' \code{b2}.
#'
#' See also \code{\link{regressor_matrix}},
#' \code{\link{covariance_matrix}}, and
#' \code{\link[tsmvr]{tsmvr_solve}}.
#'
#' @references
#' \insertRef{MRCE}{tsmvr}
#'
#' @export
make_data <- function(n, p, q, b1 = 0.2, b2 = 0.5, sigma = 1,
                      rho_x = 0.6, type = "AR1", rho_err = 0.7, h = 0.9,
                      n_edge = 1, shift = 1,  power = 1, zero_appeal = 1,
                      g = 4, diag_val = 1, edge_val = 0.3,
                      reps = 1, seed = NULL) {
  stopifnot(
    n %% 1 == 0, n > 0, p %% 1 == 0, p > 0, q %% 1 == 0, q > 0,
    is.numeric(n), is.numeric(p), is.numeric(q), sigma >= 0,
    rho_x >= 0, rho_x < 1, type %in% c("AR1", "FGN", "SFN", "HUB"),
    rho_err >= 0, rho_err < 1, h >= 0, h < 1, power > 0, zero_appeal > 0,
    n_edge > 0, shift >= 0,
    g %% 1 == 0, g > 0, g <= q, diag_val > 0, #edge_val >=  0,
    reps %% 1 == 0, reps > 0
  )

  # set.seed(seed)

  # --------------------------------------------------------------------
  # The code creates rep datasets according to the multivariate model:
  #                            Y = X*B + E
  # --------------------------------------------------------------------

  # Set up random seeds for the different random draws.
  if(is.null(seed)) {
    temp_seed_1 = seed
    temp_seed_2 = seed
    temp_seed_3 = seed
  } else {
    temp_seed_1 = seed
    temp_seed_2 = seed + 1
    temp_seed_3 = seed + 2
  }

  # Calculate the auxillary and error covariance matrices --------------
  # Sigma_x <- covariance_matrix(
  #     p, type = type, rho = rho_x, h = h, power = power,
  #     zero_appeal = zero_appeal, n_edge = n_edge, shift = shift
  # )
  Sigma_x <- covariance_matrix(
    p, type = 'AR1', rho = rho_x
  )
  Sigma_err <- covariance_matrix(
    q, type = type, rho = rho_err,
    h = h,
    power = power, zero_appeal = zero_appeal, n_edge = n_edge, shift = shift,
    g = g, diag_val = diag_val, edge_val = edge_val
  )

  # Calculate a random (list of draws of the) dataset -----------------
  X <- mvrnorm(n, rep(0, p), Sigma_x$covariance, seed = temp_seed_1)[[1]]
  B <- regressor_matrix(p, q, b1, b2, seed = temp_seed_2)
  XB <- X %*% B
  E.list <- mvrnorm(
    n, rep(0, q), Sigma_err$covariance, reps, seed = temp_seed_3
   )
  data.list <- as.list(rep(list(NULL), reps))
  for (i in 1:reps) {
    Y = XB + sigma * E.list[[i]]
    data.list[[i]]$X = X
    data.list[[i]]$B = B
    data.list[[i]]$Y = Y
    data.list[[i]]$E = E.list[[i]]
    data.list[[i]]$Sigma = Sigma_err$covariance
    data.list[[i]]$Omega = Sigma_err$precision
    data.list[[i]]$Sigma_x = Sigma_x
    # data.list[[i]]$Sigma_r <- crossprod(Y-XB)
  }

  return(data.list)
}
