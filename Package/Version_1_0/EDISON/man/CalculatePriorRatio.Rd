\name{CalculatePriorRatio}
\alias{CalculatePriorRatio}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Calculates the network prior ratio.
}

\description{
This function calculates the ratio of the network structure priors for a 
structure move.
}
\usage{
CalculatePriorRatio(method, q, lambda, network.info)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{method}{Indicates which prior to use: \code{'poisson'} for the standard
Poisson prior (no information sharing), \code{'exp_soft'} or \code{'exp_hard'}
for the exponential information sharing prior with soft or hard sharing among nodes 
and \code{'bino_soft'} or \code{'bino_hard'} for the binomial information sharing
prior with soft or hard sharing among nodes.
}
  \item{q}{Number of nodes in the network.
}
  \item{lambda}{Vector of lambda hyperparameter values for each network (needed
for the Poisson prior).
}
  \item{network.info}{The network information collected using \code{\link{CollectNetworkInfo}}.
}
}
\value{
Returns the ratio of the network structure priors for the proposed structure move.
}
\references{
For more information on the network structure priors, see:

Husmeier et al. (2010), "Inter-time segment information sharing for 
non-homogeneous dynamic Bayesian networks", NIPS.

Dondelinger et al. (2012), "Non-homogeneous dynamic Bayesian networks with 
Bayesian regularization for inferring gene regulatory networks with gradually 
time-varying structure", Machine Learning.
}
\author{Frank Dondelinger
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{CalculateLikelihoodRatio}}
}
