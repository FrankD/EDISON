\name{psrf_check_hyper}
\alias{psrf_check_hyper}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Checks the potential scale reduction factor for the hyperparameters.
}
\description{
This function checks the potential scale reduction factors for the hyperparameters of the information sharing priors.
}
\usage{
psrf_check_hyper(params, num_it)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{params}{
Matrix of hyperparameters.
}
  \item{num_it}{
Number of iterations/samples.
}
}
\value{
Returns the maximum PSRF value.
}
\references{
Gelman and Rubin (1992) Inference from iterative simulation using multiple sequences, Statistical Science.
}
\author{
Frank Dondelinger
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{psrf}}, \code{\link{psrf_check}}
}
