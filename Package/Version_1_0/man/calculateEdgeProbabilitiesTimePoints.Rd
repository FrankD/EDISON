\name{calculateEdgeProbabilitiesTimePoints}
\alias{calculateEdgeProbabilitiesTimePoints}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Calculate the edge posterior probabilities for each timepoint.
}
\description{
This function calculates the marginal posterior edge probabilities of the network at each timepoint.
}
\usage{
calculateEdgeProbabilitiesTimePoints(network.samples, cps, 

numNodes)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{network.samples}{Collection of network and changepoint samples of the MCMC simulation,
as obtained by \code{\link{EDISON.run}}, \code{\link{runDBN}}.
}
  \item{cps}{Changepoint vector.
}
\item{numNodes}{Number of nodes in the network.}
}
\value{
A list of length equal to the number of timepoints, where each entry contains a matrix 
of size NumNodes by NumNodes with the marginal posterior edge probabilities of the network
at this timepoint. 
}
\author{
Frank Dondelinger
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{calculateEdgeProbabilities}}, 

\code{\link{calculateEdgeProbabilitiesSegs}}
}
