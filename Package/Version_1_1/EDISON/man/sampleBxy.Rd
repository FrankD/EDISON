\name{sampleBxy}
\alias{sampleBxy}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Sample regression coefficients.
}
\description{
This function samples the regression coefficients given the current state of the MCMC simulation.
}
\usage{
sampleBxy(xi, y, Sig2, delta2)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{xi}{
Response data.
}
  \item{y}{
Target data.
}
  \item{Sig2}{
Sigma squared.
}
  \item{delta2}{
Signal-to-noise hyperparameter.
}
}
\value{
The regression parameters.
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

