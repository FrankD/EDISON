\name{CalculateChanges}
\alias{CalculateChanges}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Function to calculate the number of differences between adjaccent network segments.
}
\description{
This function takes the current network structure, compares each segment 
to the next one, and calculates the number of changes. If soft information 
sharing among nodes is active, then this procedure is only done for the 
current target node.
}
\usage{
CalculateChanges(network.info, node.sharing)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{network.info}{The network information collected by function \code{\link{CollectNetworkInfo}}.
}
  \item{node.sharing}{Specifies the type of information sharing among nodes:
\code{'soft'} or \code{'hard'}.
}
}
\value{
Returns a vector with 4 elements: the number of coinciding edges, the number of 
edges in the previous segment that are absent in the next one, the number of
edges in the next segment that are absent in the previous one and the number
of coinciding non-edges.
}
\author{Frank Dondelinger
}