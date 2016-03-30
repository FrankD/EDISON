calculateEdgeProbabilitiesSegs <-
function(prob.networks, cps, numNodes) {
  prob.networks.segs = list() 

  for(cp.i in 2:length(cps)) {
    timePoints = cps[cp.i-1]:(cps[cp.i]-1)
    prob.networks.segs[[cp.i-1]] = matrix(0, numNodes, numNodes) 
  
    for(t in timePoints) {
      prob.networks.segs[[cp.i-1]] = prob.networks.segs[[cp.i-1]] +
        prob.networks[[t]] 
    }
  
    prob.networks.segs[[cp.i-1]] = prob.networks.segs[[cp.i-1]]/length(timePoints)
  }
  
  return(prob.networks.segs)
}

