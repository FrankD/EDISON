\name{HyperparameterMove}
\alias{HyperparameterMove}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Make a hyperparameter move.
}
\description{
This function makes a hyperparameter move for the information sharing prior selected (or
no move if no information sharing prior is selected).
}
\usage{
HyperparameterMove(method, network.info, GLOBvar, 

hyper.proposals)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{method}{
The information sharing method used: \code{'poisson'} for the Poisson prior (no information
sharing), \code{'exp_soft'} and \code{'exp_hard'} for the exponential information sharing
prior with soft or hard information sharing among nodes, respectively, \code{'bino_soft'} and
\code{'bino_hard'} for the binomial information sharing prior with soft or hard information
sharing among nodes, respectively.
}
  \item{network.info}{
Network information collected using \code{\link{CollectNetworkInfo}}.
}
  \item{GLOBvar}{
Global variables used during the MCMC.
}
  \item{hyper.proposals}{
Proposal width for hyperparameters.
}
}
\value{
List summing up the result of the hypermove. Contains at least:
\item{move.made}{Whether a hyperparameter move has been made.}
\item{network.info}{The network information, possibly updated if the hyperparameter move was
made and accepted.}
May contain further elements depending on the type of information sharing prior used. See 
the prior-specific functions \code{\link{ExpHyperMove}} and \code{\link{BinoHyperMove}} for 
details.
}
\references{
For information about the information sharing priors, see:

Husmeier et al. (2010), "Inter-time segment information sharing for 
non-homogeneous dynamic Bayesian networks", NIPS.

Dondelinger et al. (2012), "Non-homogeneous dynamic Bayesian networks with 
Bayesian regularization for inferring gene regulatory networks with gradually 
time-varying structure", Machine Learning.
}
\author{
Frank Dondelinger
}
%% ~Make other sections like Warning with \section{Warning }{....} ~

