\name{ExpHyperRatioTarget}
\alias{ExpHyperRatioTarget}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Calculates the ratio of an exponential hyperparameter move.
}
\description{
This function calculates the acceptance ratio of a level-1 hyperparameter move for
a given target node.
}
\usage{
ExpHyperRatioTarget(beta.proposed, beta.old, target.net, 

self.loops)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{beta.proposed}{
Proposed new hyperparameter value.
}
  \item{beta.old}{
Previous value of hyperparameter beta.
}
  \item{target.net}{
Network segments for the target node associated with this hyperparameter value.
}
  \item{self.loops}{
\code{'TRUE'} if self-loops are acceptable, \code{'FALSE'} otherwise.
}
}
\value{
Returns the ratio of the exponential prior with the previous hyperparameter value and the 
proposed new hyperparameter value.
}
\references{
For information about the exponential information sharing prior, see:

Husmeier et al. (2010), "Inter-time segment information sharing for 
non-homogeneous dynamic Bayesian networks", NIPS.

Dondelinger et al. (2012), "Non-homogeneous dynamic Bayesian networks with 
Bayesian regularization for inferring gene regulatory networks with gradually 
time-varying structure", Machine Learning.
}
\author{
Frank Dondelinger
}


\seealso{
\code{\link{ExpHyperMove}}
}
