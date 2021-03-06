% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/precision_sfn.R
\name{precision_sfn}
\alias{precision_sfn}
\title{Precision matrix of scale free network}
\usage{
precision_sfn(q, n_edge = 1, shift = 1, power = 1, zero_appeal = 1)
}
\arguments{
\item{q}{square dimension of covariance matrix (positive integer)}

\item{n_edge}{Barabasi algorithm number of edges per step for SFN covariance matrix (positive integer)}

\item{shift}{eigenvalue shift parameter for SFN covariance matrix (\code{shift} > 0) (ensures matrix is PSD) )}

\item{power}{scaling power for SFN covariance matrix (positive numeric)}

\item{zero_appeal}{Barabasi algorithm baseline attractiveness for SFN covariance matrix (positive numeric)}
}
\value{
Returns an FGN covariance matrix.
}
\description{
Constructs a scale free network (SFN) precision matrix according
to the Barabasi algorithm.
}
\note{
See also \code{\link[igraph]{get.adjacency}},
\code{\link[igraph]{barabasi.game}}, and
\code{\link[igraph]{sample_pa}}.
}
\references{
\insertRef{chen2016high}{tsmvr}

\insertRef{barabasi}{tsmvr}
}
