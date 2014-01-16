calculateCPPGlobal <-
function(prob.cps) {
  global.cps = matrix(0, 1, dim(prob.cps)[2])
  
  for(node in 1:dim(prob.cps)[1]) {
    global.cps = global.cps + prob.cps[node,] - global.cps * prob.cps[node,]
  }
  
  return(global.cps)
}

