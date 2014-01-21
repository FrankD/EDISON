\name{sampleK}
\alias{sampleK}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Sample initial number of changepoints.
}
\description{
This function samples the initial number of changepoints from a sparse Poisson prior.
}
\usage{
sampleK(mini, maxi, lambda, nb)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{mini}{
Minimum value.
}
  \item{maxi}{
Maximum value.
}
  \item{lambda}{
Parameter of the Poisson distribution.
}
  \item{nb}{
Number of values to sample.
}
}
\value{
The sampled number of changepoints.
}
\references{
For more information on the prior choice and sampling, see:

Dondelinger et al. (2012), "Non-homogeneous dynamic Bayesian networks with 
Bayesian regularization for inferring gene regulatory networks with gradually 
time-varying structure", Machine Learning.
}
\author{
Sophie Lebre
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

