NetworkProbExp <-
function(network.info) {
  # Calculate the (log) probability P(M_i|M_{i-1})*P(M_{i+1}|M_i), where M_i is
  # network segment being changed.
  #
  # Args:
  #   net:          The network being changed
  #   network_info: The network structures and associated information.
  #             network.info$nets         - Structure of all segments 
  #             network.info$prior.params - Beta parameters for all segments
  #             network.info$segment      - Segment being changed
  #             network.info$target       - Target node whose edge is being changed
  #             network.info$parent       - Parent being changed
  # Returns:
  #   P(M_i|M_{i-1})*P(M_{i+1}|M_i)
  
  target = network.info$target

  suff.statistics = CalculateChanges(network.info, 'soft')
 
  differences = suff.statistics[2] + suff.statistics[3]
  
  beta = network.info$prior.params[target]
   
  logprior = -beta*differences
 
  return(logprior)
}

