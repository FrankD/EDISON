\name{psrf_check}
\alias{psrf_check}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Check the potential scale reduction factors for all parameters (edges).
}
\description{
This function treats the edges of the network as parameters, calculates their potential scale reduction factors and returns the highest value.
}
\usage{
psrf_check(params, q, k_max, num_it)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{params}{Matrix of parameters.
}
  \item{q}{
Number of nodes.
}
  \item{k_max}{
Number of segments.
}
  \item{num_it}{
Number of iterations/samples.
}
}
\value{
Returns the highest PSRF value.
}
\references{
Gelman and Rubin (1992) Inference from iterative simulation using multiple sequences, Statistical Science.
}
\author{
Frank Dondelinger
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{psrf}}, \code{\link{psrf_check_hyper}}
}
