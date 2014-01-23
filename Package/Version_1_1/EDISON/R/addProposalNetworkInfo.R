addProposalNetworkInfo <- function(network.info, newS, E) {
  # Update network info with proposed new network (assumes no changes to the changepoints)
  
  if(any(E != network.info$cps[[network.info$target]])) {
    network.info = NULL
  } else {
    
    global.update = newS[network.info$global.mapping[network.info$target,],,drop=FALSE]
    
    network.info$new.nets = lapply(1:length(network.info$nets),
      function(i) {
        net.temp = network.info$nets[[i]]
        net.temp[,network.info$target] = global.update[i,]
        return(net.temp)
      })
  }
  
  return(network.info)
}