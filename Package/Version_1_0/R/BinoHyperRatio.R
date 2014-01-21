BinoHyperRatio <-
function(params.proposed, changed, node.sharing,
                                network.info) {
  # Calculates the acceptance ratio of a level-1 hyperparameter move for the 
  # soft binomial prior.
  #
  # Args:
  #  params.proposed: Proposed new hyperparameter value
  #  changed:         Index of the changed parameter
  #  node.sharing:    Indicator flag for soft or hard information sharing among 
  #                   nodes
  #  network_info:    The network structures and associated information.
  #             network.info$nets         - Structure of all segments 
  #             network.info$target.nets  - Structure of all segments in per-node
  #                                         form (easier to use in some
  #                                         situations)
  #             network.info$prior.params - Shared hyperparameters
  #             network.info$segment      - Segment being changed
  #             network.info$target       - Target node whose edge is being 
  #                                        changed
  #             network.info$parent       - Parent being changed
  #
  # Returns:
  #   Acceptance ratio
  
  
  if(node.sharing == 'hard') {
    logprior.old = NetworkProbBino(network.info, node.sharing)
  
    network.info$prior.params[changed] = params.proposed
    
    logprior.new = NetworkProbBino(network.info, node.sharing)
  } else {
    network.info.old = network.info
    network.info.new = network.info
    
    network.info.new$prior.params[changed] = params.proposed
    
    logprior.old = 0
    logprior.new = 0
    
    for(target in 1:dim(network.info$nets[[1]])[1]) {
      network.info.old$target = target
      network.info.new$target = target
      
      logprior.old = logprior.old + NetworkProbBino(network.info.old, 
                                      node.sharing)
      logprior.new = logprior.new + NetworkProbBino(network.info.new, 
                                      node.sharing)
    }
  }
  
  return(exp(logprior.new - logprior.old))
}

