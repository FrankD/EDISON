\name{sampleBinit}
\alias{sampleBinit}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Sample initial regression coefficients.
}
\description{
This function samples the initial regression coefficients for the networks.
}
\usage{
sampleBinit(Si, sig2, delta2, X, q)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Si}{
Network structure.
}
  \item{sig2}{
Sigma squared.
}
  \item{delta2}{
Signal-to-noise ratio hyperparameter.
}
  \item{X}{
Input data.
}
  \item{q}{
Number of nodes.
}
}
\value{
Returns a vector of regression coefficients.
}
\references{
For details of the regression model, see:

Dondelinger et al. (2012), "Non-homogeneous dynamic Bayesian networks with 
Bayesian regularization for inferring gene regulatory networks with gradually 
time-varying structure", Machine Learning.
}
\author{
Sophie Lebre
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

