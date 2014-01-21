\name{sampleParms}
\alias{sampleParms}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Sample initial parameters for the MCMC simulation.
}
\description{
This function samples the initial hyperparameters and parameters that are needed for the MCMC simulation.
}
\usage{
sampleParms(X, GLOBvar, HYPERvar, s_init = NULL, options)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{X}{
Input data.
}
  \item{GLOBvar}{
Global variables of the MCMC simulation.
}
  \item{HYPERvar}{
Hyperparameter variables.
}
  \item{s_init}{
Initial number of changepoints.
}
  \item{options}{
MCMC options, as given by e.g. \code{\link{defaultOptions}}.
}
}
\value{
Returns a list with elements:
\item{E}{The initial changepoint vector.}
\item{S}{The intial networks structure.}
\item{B}{The initial regression parameters.}
\item{Sig2}{The inital sigma squared variances.}
\item{betas}{The intial hyperparameters for the exponential information sharing prior.}
\item{hyper_params}{The initial hyperparameters for the binomial information sharing prior.}
}
\references{
For more information about the parameters and hyperparameters, see:

Dondelinger et al. (2012), "Non-homogeneous dynamic Bayesian networks with 
Bayesian regularization for inferring gene regulatory networks with gradually 
time-varying structure", Machine Learning.
}
\author{
Sophie Lebre

Frank Dondelinger
}
%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{init}}
}
