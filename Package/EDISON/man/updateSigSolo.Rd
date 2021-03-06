% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/updateSigSolo.R
\name{updateSigSolo}
\alias{updateSigSolo}
\title{Sample new values for sigma squared.}
\usage{
updateSigSolo(X, Y, E, Sall, Ball, Sig2, Mphase, alphad2, betad2, v0, gamma0)
}
\arguments{
\item{X}{Input response data.}

\item{Y}{Input target data.}

\item{E}{Changepoints.}

\item{Sall}{Network structure.}

\item{Ball}{Regression coefficients.}

\item{Sig2}{Current sigma squared.}

\item{Mphase}{Segment position.}

\item{alphad2}{Gamma prior hyperparameter.}

\item{betad2}{Gamma prior hyperparameter.}

\item{v0}{Inverse gamma prior hyperparameter.}

\item{gamma0}{Inverse gamma prior hyperparameter.}
}
\value{
Returns the new samples sigma squared values.
}
\description{
This function samples new values for the sigma squared variances, given the
current network structure. A univariate distribution is assumed.
}
\author{
Sophie Lebre
}
\references{
For more information about the model, see:

Dondelinger et al. (2012), "Non-homogeneous dynamic Bayesian networks with
Bayesian regularization for inferring gene regulatory networks with
gradually time-varying structure", Machine Learning.
}
\seealso{
\code{\link{updateSigMulti}}
}

