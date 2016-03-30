BinoHyperMove <-
function(network.info, node.sharing, GLOBvar) {
  # Makes a hyperparameter move (with a certain probability)
  #
  # Args:
  #   network_info: The network structures and associated information.
  #             network.info$nets         - Structure of all segments 
  #             network.info$target.nets  - Structure of all segments in per-node
  #                                         form (easier to use in some
  #                                         situations)
  #             network.info$prior.params - Shared hyperparameters
  #             network.info$segment      - Segment being changed
  #             network.info$target       - Target node whose edge is being 
  #                                        changed
  #             network.info$parent       - Parent being changed
  #   node.sharing: Indicator flag for soft or hard sharing between nodes
  #   neg.fixed:    Keep alpha bar, gamma bar fixed at 1.
  #   pos.same:     Alpha and gamma have the same value. 
  #
  # Returns:
  #   Structure containing the updated hyperparameter
  move.made = 0
  accept = 0
  # Random value for deciding which hyperparameter move to make    
  u = runif(1,0,1)
  
  # Determine which parameter the move applies to
  
  changed = sample(1:length(network.info$prior.params), 1)  
  
  params.old = network.info$prior.params[changed]
  params.new = params.old
  
  # Assume uniform level-2 hyperprior 
  # Level-1 hyperparameter move
  if(u > (1-GLOBvar$pp.l1) && length(network.info$nets) > 1) {
    move.made = 1

    params.proposed = ProposeDiscrete(params.old, 5, 100)
  
    r = BinoHyperRatio(params.proposed, changed, node.sharing,
                      network.info)
    
    u1 = runif(1,0,1)
   
    if(u1 < r) {
      params.new = params.proposed
      accept = 1
    }
  }
  
  network.info$prior.params[changed] = params.new

  return(list(move=5, move.made=move.made, network.info=network.info, 
    accept=accept))
    
}

