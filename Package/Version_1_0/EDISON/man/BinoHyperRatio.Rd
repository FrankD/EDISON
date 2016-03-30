\name{BinoHyperRatio}
\alias{BinoHyperRatio}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Calculates the MH ratio of the binomial prior. 
}
\description{This function calculates the ratio of the binomial information sharing 
prior with the proposed new hyperparameter values, and the binomial prior with 
the current hyperparameter values.
}
\usage{
BinoHyperRatio(params.proposed, changed, node.sharing, 

network.info)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{params.proposed}{The new proposed hyperparameter values for the binomial 
prior.
}
  \item{changed}{Gives the index of the parameter that has changed.
}
  \item{node.sharing}{Type of information sharing among nodes: \code{'soft'} or
\code{'hard'}.
}
  \item{network.info}{
The network information as collected by \code{\link{CollectNetworkInfo}}.
}
}


\value{
This function returns a number greater than zero which represents the 
ratio of binomial priors.
}

\references{
For information about the binomial information sharing prior, see:

Husmeier et al. (2010), "Inter-time segment information sharing for 
non-homogeneous dynamic Bayesian networks", NIPS.

Dondelinger et al. (2012), "Non-homogeneous dynamic Bayesian networks with 
Bayesian regularization for inferring gene regulatory networks with gradually 
time-varying structure", Machine Learning.
}

\author{Frank Dondelinger
}


\seealso{
\code{\link{BinoHyperMove}}
}
