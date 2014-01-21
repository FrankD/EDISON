NetworkRatioBino <-
function(network.info, node.sharing) {
  # Calculate the ratio of probabilities when applying one edge change to a 
  # network segment
  #
  # Args:
  #   network_info: The network structures and associated information.
  #             network.info$nets         - Structure of all segments 
  #             network.info$prior.params - Hyperparameters alpha, alpha bar, 
  #                                         gamma, gamma bar of the binomial prior
  #             network.info$segment      - Segment being changed
  #             network.info$target       - Target node whose edge is being changed
  #             network.info$parent       - Parent being changed
  #  node.sharing: Indicator flag to decide between 'hard' and 'soft' coupling 
  #                over nodes.
  # Returns:
  #   Ratio of the structure priors

  logprior.old = NetworkProbBino(network.info, 
                     node.sharing)
  
  # Set the network to the new (proposed network). 
  network.info$nets = network.info$new.nets
                   
  logprior.new = NetworkProbBino(network.info, node.sharing)
   
  return(exp(logprior.new - logprior.old));
}

