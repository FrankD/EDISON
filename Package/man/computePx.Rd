\name{computePx}
\alias{computePx}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Compute projection matrix.
}
\description{This function computes the projection matrix that is needed for 
calculation of the likelihood.
}
\usage{
computePx(len, x, delta2)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{len}{
Delimiting breakpoints.
}
  \item{x}{
The observations of x in the corresponding state.
}
  \item{delta2}{
Signal-to-noise ratio hyperparameter.
}
}
\value{
The projection matrix.
}
\references{
For more information about the hyperparameters and the functional form of the 
likelihood, see:

Dondelinger et al. (2012), "Non-homogeneous dynamic Bayesian networks with 
Bayesian regularization for inferring gene regulatory networks with gradually 
time-varying structure", Machine Learning.
}
\author{Sophie Lebre
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{CalculateLikelihoodRatio}}
}
