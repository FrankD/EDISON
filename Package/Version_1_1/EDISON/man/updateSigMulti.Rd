\name{updateSigMulti}
\alias{updateSigMulti}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Update sigma squared variances.
}
\description{
This function samples new values for the sigma squared variances, given the current network structure. A multivariate distribution is assumed.
}
\usage{
updateSigMulti(phase, X, Y, E, Sall, Ball, Sig2, Mphase, 

alphad2, betad2, v0, gamma0)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{phase}{
Current segment.
}
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
Current sigma squared values.
}
  \item{Mphase}{
Segment positions.
}
  \item{alphad2}{
Hyperparameter for gamma prior.
}
  \item{betad2}{
Hyperparameter for gamma prior.
}
  \item{v0}{
Hyperparameter for inverse gamma prior.
}
  \item{gamma0}{
Hyperparameter for inverse gamma prior.
}
}
\value{
The new samples sigma squared values.
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
\code{\link{updateSigSolo}}
}
