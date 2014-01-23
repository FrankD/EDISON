\name{ExpHyperMove}
\alias{ExpHyperMove}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Makes an exponential hyperparameter move.
}
\description{
This function tries to make a level-1 or level-2 hyperparameter move for the exponential 
prior
}
\usage{
ExpHyperMove(network.info, node.sharing, GLOBvar, 

hyper.proposals)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{network.info}{
The network information collected by \code{\link{CollectNetworkInfo}}.
}
  \item{node.sharing}{
The type of information sharing among nodes: \code{'soft'} or \code{'hard'}.
}
  \item{GLOBvar}{
Collection of global variables of the MCMC.
}
  \item{hyper.proposals}{
Proposal width of the hyperparameter move.
}
}
\value{
Returns a list with elements:
\item{move.made}{1 if a level-1 hyperparameter move has been made, 0 otherwise.}
\item{network.info}{Network information with updated hyperparameters if the move was 
accepted.}
\item{accept}{Whether a level-1 hyperparameter move has been accepted or not.}
\item{move.made.k}{1 if a level-2 hyperparameter move has been made, 0 otherwise.}
\item{accept.k}{Whether a level-2 hyperparameter move has been accepted or not.}
\item{move}{Type of move: 2 for a level-1 hyperparameter move, 3 for a level-2 
hyperparameter move.} 
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

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{ExpHyperRatioTarget}}
}
