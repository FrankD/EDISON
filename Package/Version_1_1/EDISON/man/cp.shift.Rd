\name{cp.shift}
\alias{cp.shift}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Makes a changepoint shift move.
}
\description{
This function makes a changepoint shift move, possibly moving one of the changepoints.
}
\usage{
cp.shift(Eall, Sall, Ball, Sig2all, X, Y, GLOBvar, HYPERvar, 

target)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
 \item{Eall}{
Changepoints: List of target nodes, where each element contains a vector of changepoints.
}
  \item{Sall}{
Network structure: List of target nodes, where each element is a NumSegs by NumNodes matrix giving the parents for the target node in each segment. A binary matrix.
}
  \item{Ball}{
Network parameters: Similar to network structure, but with regression parameters included.
}
  \item{Sig2all}{
Sigma squared parameters.
}
  \item{X}{
Response data.
}
  \item{Y}{
Target data.
}
  \item{GLOBvar}{
Global variables of the MCMC simulation.
}
  \item{HYPERvar}{
Hyperparameter variables.
}
  \item{target}{
Which target node the move is being proposed for.
}
}
\value{
A list with elements:
\item{E}{New changepoint vector for target node.}
\item{Sall}{Updated network structure.}
\item{Ball}{Updated network structure with regression parameters.}
\item{Sig2all}{Updated sigma squared.}
\item{prior.params}{Updated vector of structure prior hyperparameters.}
\item{accept}{Whether the move was accepted or not.}
\item{move}{What type of move was made. In this case \code{move=2} for a changepoint
death move.}
\item{alpha}{The acceptance ratio of the move.}
\item{estar}{The location of the removed changepoint.}
\item{k}{Hyperparameter.}

}
\references{
For more information about the different changepoint moves, see:

Dondelinger et al. (2012), "Non-homogeneous dynamic Bayesian networks with 
Bayesian regularization for inferring gene regulatory networks with gradually 
time-varying structure", Machine Learning.
}
\author{
Sophie Lebre
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{cp.birth}}, \code{\link{cp.death}}
}
