#' Precision matrix of scale free network
#'
#' Constructs a scale free network (SFN) precision matrix according
#' to the Barabasi algorithm.
#'
#' @param q square dimension of covariance matrix (positive integer)
#' @param n_edge Barabasi algorithm number of edges per step for SFN covariance matrix (positive integer)
#' @param shift eigenvalue shift parameter for SFN covariance matrix (\code{shift} > 0) (ensures matrix is PSD) )
#' @param power scaling power for SFN covariance matrix (positive numeric)
#' @param zero_appeal Barabasi algorithm baseline attractiveness for SFN covariance matrix (positive numeric)

#' @references
#' \insertRef{chen2016high}{tsmvr}
#'
#' \insertRef{barabasi}{tsmvr}
#'
#' @note
#' See also \code{\link[igraph]{get.adjacency}},
#' \code{\link[igraph]{barabasi.game}}, and
#' \code{\link[igraph]{sample_pa}}.
#'
#' @return Returns an FGN covariance matrix.
#' @export
precision_sfn <- function(q, n_edge = 1, shift = 1, power = 1, zero_appeal = 1) {
  stopifnot(
    q %% 1 == 0, q > 0, power > 0, zero_appeal > 0, n_edge > 0,
    shift >= 0
  )
  Omega <- -1*as.matrix(igraph::get.adjacency(igraph::barabasi.game(
    n = q, m = n_edge,
    power = power,
    zero.appeal = zero_appeal,
    directed = F,
    algorithm = "psumtree"
  )))
  min.lambda <- min(eigen(Omega)$values)
  if (min.lambda <= 0) Omega <- Omega + (abs(min.lambda) + shift) * diag(q)
  return(Omega)
}
