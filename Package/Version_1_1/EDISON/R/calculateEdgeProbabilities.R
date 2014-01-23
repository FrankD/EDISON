calculateEdgeProbabilities <-
function(network.samples, cps=NULL) {
  if(is.null(cps)) cps = c(2, network.samples$n+1)
  
  numNodes = length(network.samples) - 1

  prob.networks = 
    calculateEdgeProbabilitiesTimePoints(network.samples, cps, numNodes)

  prob.networks.segs = 
    calculateEdgeProbabilitiesSegs(prob.networks, cps, numNodes)
  
  return(list(probs.all=prob.networks, probs.segs=prob.networks.segs))
}

