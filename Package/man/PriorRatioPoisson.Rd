\name{PriorRatioPoisson}
\alias{PriorRatioPoisson}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Calculate network prior ratio with Poisson prior.
}
\description{This function calculates the ratio of the Poisson prior for two networks.
}
\usage{
PriorRatioPoisson(network.info, q, lambda)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{network.info}{Network information collected using \code{\link{CollectNetworkInfo}}. Note that one needs to set
\code{network.info$new.nets}.
}
  \item{q}{Number of nodes in the network.
}
  \item{lambda}{Vector of lambda hyperparameters for each network.
}
}
\value{
Returns the ratio [prior of new network]/[prior of old network].
}
\references{
For more information on the network structure priors, see:

Dondelinger et al. (2012), "Non-homogeneous dynamic Bayesian networks with 
Bayesian regularization for inferring gene regulatory networks with gradually 
time-varying structure", Machine Learning.

}
\author{Frank Dondelinger
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{CalculatePriorRatio}}
}
