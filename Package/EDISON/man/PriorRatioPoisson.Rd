% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/PriorRatioPoisson.R
\name{PriorRatioPoisson}
\alias{PriorRatioPoisson}
\title{Calculate network prior ratio with Poisson prior.}
\usage{
PriorRatioPoisson(network.info, q, lambda)
}
\arguments{
\item{network.info}{Network information collected using
\code{\link{CollectNetworkInfo}}. Note that one needs to set
\code{network.info$new.nets}.}

\item{q}{Number of nodes in the network.}

\item{lambda}{Vector of lambda hyperparameters for each network.}
}
\value{
Returns the ratio [prior of new network]/[prior of old network].
}
\description{
This function calculates the ratio of the Poisson prior for two networks.
}
\author{
Frank Dondelinger
}
\references{
For more information on the network structure priors, see:

Dondelinger et al. (2012), "Non-homogeneous dynamic Bayesian networks with
Bayesian regularization for inferring gene regulatory networks with
gradually time-varying structure", Machine Learning.
}
\seealso{
\code{\link{CalculatePriorRatio}}
}

