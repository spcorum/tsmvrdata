#' Precision matrix of scale free network
#'
#' Constructs a Hub network precision matrix
#'
#' @param q square dimension of precision matrix (positive numeric integer)
#' @param g number of hubs, (numeric integer st. 1 <= g <= q)
#' @param diag_val values of diagonal entries HUB graph precision matrix (non-negative numeric)
#' @param edge_val values of HUB graph network edges
#' @references
#' \insertRef{chen2016high}{tsmvr}
#'
#' @return Returns an Hub graph precision matrix
#' @export
precision_hub <- function(q, g, diag_val = 2, edge_val=0.3) {
  stopifnot(
    q %% 1 == 0, q > 0,
    g %% 1 == 0, g > 0, g <= q,
    diag_val > 0#, edge_val >=  0
  )
  Omega = matrix(rep(0,q^2),q,q)
  start_vec = seq(from=1,to=10,by=g)
  for (i in 1:length(start_vec)) {
    stop = min(start_vec[i] + g - 1,q)
    center = start_vec[i] + floor(g/2)
    center = start_vec[i]
    for (j in start_vec[i]:stop) {
      for (k in start_vec[i]:stop) {
        if (k <= q) if (k == center) Omega[j,k] = edge_val
        if (j <= q) {
          if (j == center) Omega[j,k] = edge_val
          if (j == k) Omega[j,k] = 1
        }
      }
    }
  }
  Omega = Omega + (diag_val-1)*diag(q)
  if (eigen(Omega)$values[q] <= 0)
    warning('Hub graph precision matrix is not positive definite.')
  return(Omega)
}


# Omega = precision_hub(q=10, g=6, 0.6)
# q = 10
# g = 4
# Omega = precision_hub(q=q, g=g, edge_val=0.3, gamma=1e-6)
# Omega
# print(sum(Omega!=0))
