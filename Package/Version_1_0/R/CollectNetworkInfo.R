CollectNetworkInfo <-
function(Sall, Eall, prior.params, posPhase, target, 
                               q, self.loops, k) {
  # Collects information about the current network segments and hyperparameters
  # for the information sharing priors
  #
  # Args:
  #   Sall:           Structure of all segments
  #   Eall:           Position of segment boundaries
  #   prior.params:   Parameters for the information sharing prior
  #   posPhase:       Segment being changed
  #   target:         Target node whose edge is being changed
  #
  # Returns:
  #   network_info: The network structures and associated information.
  #             network.info$nets         - Structure of all segments
  #             network.info$new.nets     - Structure of all segments after
  #                                         applying the current move 
  #             network.info$target.nets  - Structure of all segments in per-node
  #                                         form (easier to use in some
  #                                         situations)
  #             network.info$prior.params - Parameters for the information
  #                                         sharing prior                                        
  #             network.info$segment      - Segment being changed
  #             network.info$target       - Target node whose edge is being 
  #                                         changed
  #             network.info$self.loops   - Indicator variable showing whether
  #                                         self loops are allowed.
  
  if(any(Sall[[1]][Sall[[1]] != 0] != 1)) 
     stop('Illegal Network Structure in CollectNetworkInfo.') 
  
  network.info = list()
  
  # Convert from list of target nodes to list of segments (easier to 
  # calculate segment similarity)
  converted = convert_nets(Sall, Eall)
  
  network.info$nets = converted$B_nets; 
  network.info$segment = posPhase; network.info$target = target
  network.info$target.nets = Sall
  network.info$prior.params = prior.params
  network.info$self.loops = self.loops
  network.info$k = k

  for(i in 1:length(network.info$nets)) {
    network.info$nets[[i]] = network.info$nets[[i]][1:q,]
  }
  
  for(j in 1:length(network.info$target.nets)) {
    network.info$target.nets[[j]] = network.info$target.nets[[j]][,1:q]
  }
  
  network.info$new.nets = network.info$nets

  return(network.info)
}

