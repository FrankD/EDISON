\name{simulateNetwork}
\alias{simulateNetwork}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Generate network and simulate data.
}
\description{
This function generates a random network with structure changepoints (or takes one as input) and simulated data from it using a regression model.
}
\usage{
simulateNetwork(l = 100, min_phase_length = 10, k_bar = 10, 

q = 10, lambda_2 = 0.45, noise = 0.25, net = NULL, 

lambda_3 = 2, spacing = 0, gauss_weights = FALSE, 

same = FALSE, changes = "sequential", fixed = FALSE, 

cps = NULL, saveFile = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{l}{
Length of the time series.
}
  \item{min_phase_length}{
Minimum segment length.
}
  \item{k_bar}{
Maximum number of changepoints.
}
  \item{q}{
Number of nodes.
}
  \item{lambda_2}{
Average number of parents for each node in the network (parameter for a Poisson distribution).
}
  \item{noise}{
Standard deviation of the Gaussian observation noise. Can be constant, or segment specific (in which case the number of changepoints needs to be fixed and the noise needs to be a vector of the same length).
}
  \item{net}{
Input network, can be \code{NULL} if a new network should be generated.
}
  \item{lambda_3}{
Average number of structure changes between two segments (parameter for a Poisson distribution).
}
  \item{spacing}{
\code{1} if segments are equally spaced, \code{0} if they are spaced randomly (subject to the constraints of min_phase_length).
}
  \item{gauss_weights}{
\code{1} if edge weights in the network are drawn from N(0, 1), \code{0} if they are fixed to be 1.
}
  \item{same}{
\code{1} if the networks should all be the same (no changes), \code{0} otherwise.
}
  \item{changes}{
\code{'sequential'} if the changes happen sequentially (i.e. changes at segment i are applied to segment i-1), \code{'hierarchical'} if the changes happen with respect to a hypernetwork (i.e. changes at segment i are applied to segment 0).
}
  \item{fixed}{
\code{T} if the changepoint locations are fixed, \code{F} if they should be sampled.
}
  \item{cps}{
Changepoint locations (if they are fixed).
}
  \item{saveFile}{If not \code{NULL}, indicates the filename for saving the 
  output in R data format.}
}
\value{
A list with elements:
\item{sim_data}{A matrix of length NumNodes by NumTimepoints containing the simulated data from the regression model.}
\item{epsilon}{Changepoint vector.}
\item{k}{Number of changepoints.}
\item{network}{The network, a list of length NumSegs, where each element is a NumNodes by NumNodes matrix.}
\item{noise}{The standard deviation of the applied Gaussian noise.}
}
\author{
Frank Dondelinger
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{generateNetwork}}
}
\examples{
# Generate random network and simulate data with default parameters
dataset = simulateNetwork()

# Generate random network and simulate data with an average of 
# 1 change per node among network segments
dataset = simulateNetwork(lambda_3=1)

# Generate random network and simulate data with an average of 
# 1 change per node among network segments and standard deviation 
# of the Gaussian observation noise 0.5
dataset = simulateNetwork(lambda_3=1, noise=0.5)

# Generate random network with default parameters
network = generateNetwork()

# Simulate data using generated network
dataset = simulateNetwork(net=network)

# Generate random network with 4 changepoints and 15 nodes, 
# with changepoints distributed over a timeseries of length 50
network = generateNetwork(l=50, q=15, fixed=TRUE, k_bar=4)

# Simulate data of length 50 using generated network
dataset = simulateNetwork(net=network)
}
