\name{ProposeDiscrete}
\alias{ProposeDiscrete}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Propose a new discrete value.
}
\description{
This function proposes a new discrete parameter, based on the previous value, within the given
proposal range, making sure that the maximum range is not exceeded.
}
\usage{
ProposeDiscrete(params.old, proposal.range, max.range)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{params.old}{
Old parameter value (an integer).
}
  \item{proposal.range}{
Range for new proposal (an integer).
}
  \item{max.range}{
Maximum value for new proposal (an integer).
}
}
\value{
Returns the new proposed parameter, which will be an integer in the range [0, 
\code{max.range}], and within at most \code{proposal.range} of \code{params.old}.
}
\author{Frank Dondelinger
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{proposeContinuous}}
}
\examples{
# Previous parameter value
param = rpois(1, 5)

# Propose new value within range [0, 10], with proposal width 2
new.param = ProposeDiscrete(param, 2, 10)
}
