NetworkRatioExp <-
function(network.info) {
  # Calculate the ratio of probabilities when applying one edge change to a 
  # network segment
  #
  # Args:
  #   network_info: The network structures and associated information.
  #             network.info$nets         - Structure of all segments 
  #             network.info$prior.params - Beta parameters for all segments
  #             network.info$segment      - Segment being changed
  #             network.info$target       - Target node whose edge is being changed
  #             network.info$parent       - Parent being changed
  # Returns:
  #   Ratio of the structure priors

  logprior.old = NetworkProbExp(network.info)
  
  network.info$nets = network.info$new.nets
    
  logprior.new = NetworkProbExp(network.info)
  
  return(exp(logprior.new - logprior.old));
}

