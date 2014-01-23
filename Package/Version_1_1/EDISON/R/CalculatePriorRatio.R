CalculatePriorRatio <-
function(method, q, lambda, network.info) {
  # Calculate the ratio of the network structure priors for a structure move.
  #
  # Args:
  #   method: String showing which prior to use:
  #             "poisson"  - Standard poisson prior (no information sharing)
  #             "exp_soft" - Exponential sequential prior with soft information
  #                          sharingi
  #   q:      Number of nodes in the network
  #   lambda: Vector of lambda values for each network (needed for Poisson prior)
  #   network_info: The network structures and associated information.
  #             network.info$nets     - Structure of all segments 
  #             network.info$betas    - Beta parameters for all segments
  #             network.info$segment  - Segment being changed
  #             network.info$target   - Target node whose edge is being changed
  #             network.info$parent   - Parent being changed
  # Returns:            
  #   Ratio of the structure priors.
  
  if(method == 'poisson') {
    prior.ratio = PriorRatioPoisson(network.info, q, lambda)
  } else if(method == 'exp_soft' || method == 'exp_hard') {
    prior.ratio = NetworkRatioExp(network.info)
  } else if(method == 'bino_soft') {
    prior.ratio = NetworkRatioBino(network.info, 'soft')
  } else if(method == 'bino_hard') {
    prior.ratio = NetworkRatioBino(network.info, 'hard')
  } 
  
  return(prior.ratio)
}

