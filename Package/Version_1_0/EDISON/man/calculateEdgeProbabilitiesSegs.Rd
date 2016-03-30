\name{calculateEdgeProbabilitiesSegs}
\alias{calculateEdgeProbabilitiesSegs}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Calculate edge probabilities for fixed segments.
}
\description{
This function calculates the marginal posterior probabilities for the edges in each network for the 
specified segments.
}
\usage{
calculateEdgeProbabilitiesSegs(prob.networks, cps, numNodes)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{prob.networks}{List containing the marginal posterior probabilities for the edges of each 
network at each timepoint, from \code{\link{calculateEdgeProbabilitiesTimePoints}}.
}
  \item{cps}{
Changepoints defining the segments for which the edge probabilities should be calculated. Note that 
these are global changepoints that apply to the whole network.
}
\item{numNodes}{Number of nodes in the network.}
}
\value{
Returns a list of length equal to the number of segments, with each entry containing a matrix 
of size NumNodes by NumNodes which contains the marginal edge probabilities for that segment.
}
\author{Frank Dondelinger
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{calculateEdgeProbabilities}}, 

\code{\link{calculateEdgeProbabilitiesTimePoints}}
}
