\name{fix_eigenvalues}
\alias{fix_eigenvalues}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Modify network to ensure stationarity.
}
\description{
This function ensures that the eigenvalues of the network structure matrix are smaller or equal to 1, thereby ensuring stationarity of the regression. This is done by removing edges at random until the condition is satisfied.
}
\usage{
fix_eigenvalues(network, q, gauss_weights)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{network}{
Original network structure, a matrix of size NumNodes by NumNodes.
}
  \item{q}{
Number of nodes.
}
  \item{gauss_weights}{
If \code{TRUE}, use Gaussian regression weight, if \code{FALSE} conserve original weights.
}
}
\value{
Returns a network with fewer eigenvalues than the original network, but satisfying the stationarity condition.
}
\author{
Frank Dondelinger
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{generateNetwork}}
}
