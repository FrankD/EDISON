\name{updateSigSolo}
\alias{updateSigSolo}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Sample new values for sigma squared.
}
\description{
This function samples new values for the sigma squared variances, given the current network structure. A univariate distribution is assumed.
}
\usage{
updateSigSolo(X, Y, E, Sall, Ball, Sig2, Mphase, alphad2, 

betad2, v0, gamma0)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{X}{
Input response data.
}
  \item{Y}{
Input target data.
}
  \item{E}{
Changepoints.
}
  \item{Sall}{
Network structure.
}
  \item{Ball}{
Regression coefficients.
}
  \item{Sig2}{
Current sigma squared.
}
  \item{Mphase}{
Segment position.
}
  \item{alphad2}{
Gamma prior hyperparameter.
}
  \item{betad2}{
Gamma prior hyperparameter.
}
  \item{v0}{
Inverse gamma prior hyperparameter.
}
  \item{gamma0}{
Inverse gamma prior hyperparameter.
}
}
\value{
Returns the new samples sigma squared values.
}
\references{
For more information about the model, see:

Dondelinger et al. (2012), "Non-homogeneous dynamic Bayesian networks with 
Bayesian regularization for inferring gene regulatory networks with gradually 
time-varying structure", Machine Learning.
}
\author{
Sophie Lebre
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{updateSigMulti}}
}
