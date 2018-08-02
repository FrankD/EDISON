# Test script for repeated sample time series inference
library(EDISON)

set.seed(10)

# Generate a network with 5 nodes and no changepoints
test = generateNetwork(l=10, q=5, k_bar=0)

# Generate 100 repeated measurements for 10 time points
test.data = lapply(1:100, simulateNetwork, l=10, net=test)

# Make array and put dimensions in the right order (repeats, time points, variables)
test.data.array = sapply(test.data, function(x) x$sim_data, simplify='array')
test.data.array = aperm(test.data.array, c(3,2,1))

# Specify inference with no changepoints
edison.options = defaultOptions()
edison.options$cp.fixed = TRUE

# Run EDISON
edison.test = EDISON.run(test.data.array, num.iter=100000, options=edison.options)

# Calculate posterior probabilities of the edges in the network
calculateEdgeProbabilities(edison.test)$probs.segs
