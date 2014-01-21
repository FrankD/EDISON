calculateEdgeProbabilitiesTimePoints <- 
  function(network.samples, cps, numNodes) {
  sampled = network.samples[[1]]$sampled 
  numSegs = length(cps) - 1
  
  segs = 2:cps[length(cps)]
  
  prob.networks = list() 
  
  for(seg in 1:length(segs)) {
    prob.networks[[seg]] = matrix(0, numNodes, numNodes)
  }
  
  for(sample.i in 1:(length(sampled)-1)) {
    for(target in 1:numNodes) {
      cps.temp = network.samples[[target]]$cp_samples[sample.i,]
      max.cp   = length(cps.temp) - 1
      cps.temp = cps.temp[cps.temp > 0]
      
      segs.temp = 2:cps.temp[length(cps.temp)]; seg = 1
      
      for(t in 1:length(segs.temp)) {
        
        if(t == cps.temp[seg+1]) {
          seg = seg + 1
        }
        
        segs.temp[t] = seg
      } 
      
      target.net.temp = 
        matrix(network.samples[[target]]$edge_samples[sample.i,],
               numNodes+1, max.cp)
      target.net.temp = (abs(target.net.temp) > 0)*1
      
      for(seg.i in 1:length(segs)) {
        prob.networks[[seg.i]][,target] = prob.networks[[seg.i]][,target] + 
          target.net.temp[1:numNodes, segs.temp[seg.i]] 
      }
    } 
  }
  
  for(seg.i in 1:length(segs)) {
    prob.networks[[seg.i]] = prob.networks[[seg.i]] / 
      length(sampled)
  }
  
  return(prob.networks)
}
