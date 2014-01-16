\name{NetworkRatioBino}
\alias{NetworkRatioBino}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Calculates the ratio of binomial prior probabilites.
}
\description{This function calculates the ratio of binomial prior probabilities of two networks.
}
\usage{
NetworkRatioBino(network.info, node.sharing)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{network.info}{Network information collected by function \code{\link{CollectNetworkInfo}}. 
Note that network.info$new.nets has to be set.
}
  \item{node.sharing}{
Type of coupling of hyperparameters among nodes: \code{'hard'} or \code{'soft'}.
}
}
\value{Returns the ratio of [prior of new network]/[prior of old network]. 
}
\author{
For information about the binomial information sharing prior, see:

Husmeier et al. (2010), "Inter-time segment information sharing for 
non-homogeneous dynamic Bayesian networks", NIPS.

Dondelinger et al. (2012), "Non-homogeneous dynamic Bayesian networks with 
Bayesian regularization for inferring gene regulatory networks with gradually 
time-varying structure", Machine Learning.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{NetworkProbBino}}, \code{\link{CalculatePriorRatio}}
}
