\name{calculateCPProbabilities}
\alias{calculateCPProbabilities}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Calculate the changepoint probabilities.
}
\description{This function calculates the marginal changepoint probabilities from the changepoint 
samples taken during the MCMC simulation.
}
\usage{
calculateCPProbabilities(network.samples)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{network.samples}{List of network and changepoint samples collected during the MCMC simulation
by \code{\link{EDISON.run}} and \code{\link{runDBN}}.
}
}
\value{
Returns a matrix of dimension NumNodes by NumTimePoints, where each entry contains the marginal 
posterior probability of a changepoint for that node at that timepoint.
}
\author{Frank Dondelinger
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\examples{
# Generate random gene network and simulate data from it
dataset = simulateNetwork()

# Run MCMC simulation to infer networks and changepoint locations
result = EDISON.run(dataset$sim_data, num.iter=500)

# Calculate posterior probabilities of changepoints
cps = calculateCPProbabilities(result)
}

