\name{EDISON-package}
\alias{EDISON-package}
\alias{EDISON}
\docType{package}
\title{Allows for network reconstruction and changepoint detection.
}
\description{
This package runs an MCMC simulation to reconstruct networks from time series data, using a non-homogeneous, time-varying dynamic Bayesian network. Networks segments and changepoints are inferred concurrently, and information sharing priors provide a reduction of the inference uncertainty.
}
\details{
\tabular{ll}{
Package: \tab nhDBN\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2012-06-21\cr
License: \tab GPL-2\cr
LazyLoad: \tab yes\cr
}
}
\author{
Frank Dondelinger, Sophie Lebre

Maintainer: Frank Dondelinger <frankd@bioss.ac.uk>
}
\references{
Dondelinger et al. (2012), "Non-homogeneous dynamic Bayesian networks with 
Bayesian regularization for inferring gene regulatory networks with gradually 
time-varying structure", Machine Learning.

Husmeier et al. (2010), "Inter-time segment information sharing for 
non-homogeneous dynamic Bayesian networks", NIPS.
}
\keyword{ package }
\seealso{
\code{\link[corpcor:corpcor-package]{corpcor}}

\code{\link[ARTIVA:ARTIVA-package]{ARTIVA}}
}
\examples{
# Generate random gene network and simulate data from it
dataset = simulateNetwork(l=25)

# Run MCMC simulation to infer networks and changepoint locations
result = EDISON.run(dataset$sim_data, num.iter=500)

# Calculate posterior probabilities of changepoints
cps = calculateCPProbabilities(result)

# Calculate marginal posterior probabilities of edges in the network
network = calculateEdgeProbabilities(result)

}
