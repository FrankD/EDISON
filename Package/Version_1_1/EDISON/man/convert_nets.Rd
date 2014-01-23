\name{convert_nets}
\alias{convert_nets}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Convert internal representation of networks.
}
\description{
Converts from representing the network as a list of target nodes to representing it as 
a list of segments.
}
\usage{
convert_nets(Ball, Eall)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Ball}{Input network: List of target nodes, where each element is a NumSegs by NumNodes matrix giving the parents for the target node in each segment.
}
  \item{Eall}{
Changepoints: List of target nodes, where each element contains a vector of changepoints.
}
}
\value{
List with elements:
\item{B_nets}{List of segments, where each element contains a matrix of size NumNodes
by NumNodes, representing the network for that segment.}
\item{segs}{Vector containing the global segment boundaries.}
}
\author{Frank Dondelinger
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

