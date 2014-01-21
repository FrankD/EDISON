\name{AcceptableMove}
\alias{AcceptableMove}
\title{Check if move is acceptable.}
\description{
This function takes as input a new network proposal and checks that the proposal 
does not exceed the maximum number of parents for a node, and that there are no 
self loops (if self loops have been disallowed).}
\usage{
AcceptableMove(proposal, qmax, self.loops, target)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{proposal}{The proposed network (K-by-q matrix with K segments and q parent 
sets).}
  \item{qmax}{Maximum number of parents allowed.}
  \item{self.loops}{Flag indicating whether self loops are allowed.}
  \item{target}{The current target node (only needed to find out which parent would
be the self loop).}
}
\value{
Returns \code{TRUE} if the proposed move is acceptable, \code{FALSE} otherwise.}
\author{
Frank Dondelinger}

\seealso{
\code{\link{make_structure_move}}
}