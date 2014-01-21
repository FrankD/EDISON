\name{NetworkRatioExp}
\alias{NetworkRatioExp}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Calculates the ratio of exponential network prior probabilities.
}
\description{This function calculates the ratio of exponential network information sharing prior
probabilities.
}
\usage{
NetworkRatioExp(network.info)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{network.info}{Network information collected using the function 
\code{\link{CollectNetworkInfo}}. Note that \code{network.info$new.nets} has to be set.
}
}
\value{
Returns the ratio [prior of new network]/[prior of old network].
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


\seealso{
\code{\link{NetworkProbExp}}, \code{\link{CalculatePriorRatio}}
}
