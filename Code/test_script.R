# Test script for repeated sample time series inference
library(EDISON)

set.seed(10)
# Generate some repeated samples, no changepoints
test = generateNetwork(l=10, q=5, k_bar=0)

test.data = lapply(1:100, simulateNetwork, l=10, net=test)

# Make array and put elements in the right order
test.data.array = sapply(test.data, function(x) x$sim_data, simplify='array')
test.data.array = aperm(test.data.array, c(3,2,1))

edison.test = EDISON.run(test.data.array)
