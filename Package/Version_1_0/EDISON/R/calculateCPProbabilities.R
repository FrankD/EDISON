calculateCPProbabilities <-
function(network.samples) {
  timePoints = 1:(network.samples$n+1)
  
  sampled = network.samples[[1]]$sampled 
  numNodes = length(network.samples) - 1

  prob.cps = matrix(0, numNodes, length(timePoints)) 
  colnames(prob.cps) <- timePoints

  for(sample.i in 1:length(sampled)) {
     for(target in 1:numNodes) {
       cps.temp = network.samples[[target]]$cp_samples[sample.i,]
       cps.indices = which(timePoints %in% cps.temp)
       prob.cps[target,cps.indices] = prob.cps[target,cps.indices] + 1 
  
       numSegs = length(cps.temp) - 1
     } 
  }
  
  prob.cps = prob.cps / length(sampled) 
  
  global.cps = calculateCPPGlobal(prob.cps)
  
  return(list(node.cps=prob.cps, global.cps=global.cps))
}

