\name{NetworkProbBino}
\alias{NetworkProbBino}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Calculates the prior probability of the network segments under the binomial
prior.
}
\description{
This function calculates the (log) probability of the network segments using the 
binomial information sharing prior.
}
\usage{
NetworkProbBino(network.info, node.sharing = "soft")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{network.info}{Network information collected by function \code{\link{CollectNetworkInfo}}.
}
  \item{node.sharing}{Coupling of hyperparameters among nodes: \code{'hard'} or \code{'soft'}.
}
}
\value{
Returns the log prior probability of the network segments under the binomial prior.
}

\references{
For information about the binomial information sharing prior, see:

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

\seealso{
\code{\link{NetworkRatioBino}}, \code{\link{CalculatePriorRatio}}
}
