\name{NetworkProbExp}
\alias{NetworkProbExp}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Calculates the prior probability of the network using the exponential prior.
}
\description{
This function calculates the log prior probability of the network structure. 
It uses the exponential information sharing prior. 
}
\usage{
NetworkProbExp(network.info)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{network.info}{Network information collected using the function 
\code{\link{CollectNetworkInfo}}
}
}
\value{
Returns the log prior probability of the network segments.
}
\references{
For information about the exponential information sharing prior, see:

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
\code{\link{NetworkRatioExp}}, \code{\link{CalculatePriorRatio}}
}
