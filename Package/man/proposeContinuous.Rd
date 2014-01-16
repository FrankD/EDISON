\name{proposeContinuous}
\alias{proposeContinuous}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Propose a new real hyperparameter value.
}
\description{
This function proposes a new real values hyperparameter for the information sharing prior.
}
\usage{
proposeContinuous(orig_beta, proposal_range, limit = 30)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{orig_beta}{Current value of the hyperparameter.
}
  \item{proposal_range}{Range for the new value.
}
  \item{limit}{Hard limit on the range.
}
}
\value{
Returns a new uniformly random value within \code{proposal_range} of \code{orig_beta} and limited by \code{limit}.
}
\author{
Frank Dondelinger
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{ProposeDiscrete}}
}
\examples{
# Previous parameter value
param = runif(1, 0, 1)

# Propose new value within range [0, 1], with proposal width 0.1
new.param = proposeContinuous(param, 0.1, 1)
}
