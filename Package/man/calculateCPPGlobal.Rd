\name{calculateCPPGlobal}
\alias{calculateCPPGlobal}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Calculated the global changepoint probabilities.
}
\description{This function calculates the global probability of a changepoint at each measured timepoint, using the 
node-specific probabilities.
}
\usage{
calculateCPPGlobal(prob.cps)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{prob.cps}{Node-specific changepoint probabilities, a NumNodes by NumTimepoints matrix.
}
}
\value{A matrix of length 1 by NumTimepoints, containing the global changepoint probabilities.
}
\author{Frank Dondelinger
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{\code{\link{calculateCPProbabilities}}
}

