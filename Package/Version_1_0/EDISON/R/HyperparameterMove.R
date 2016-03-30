HyperparameterMove <-
function(method, network.info, GLOBvar, hyper.proposals) {
  # Makes a hyperparameter move (with a certain probability)
  #
  # Args:
  #   method: The information sharing method used. Currently supports "poisson"
  #           (no information sharing) and "exp_soft" (sequential exponential 
  #           information sharing with soft coupling of nodes)
  #   network_info: The network structures and associated information.
  #             network.info$nets        - Structure of all segments 
  #             network.info$target.nets - Structure of all segments in per-node
  #                                        form (easier to use in some
  #                                        situations)
  #             network.info$betas       - Beta parameters for all segments
  #             network.info$segment     - Segment being changed
  #             network.info$target      - Target node whose edge is being 
  #                                        changed
  #             network.info$parent      - Parent being changed
  #
  # Returns:
  #   Structure containing the updated hyperparameter
  
  if(length(network.info$nets) == 1) {
    # Only one segment present, no hyper moves needed
    return(list(move.made=0, network.info)) 
  }

  if(method == 'poisson' || GLOBvar$hyper.fixed) {
    hyper.move = list(move.made=0, network.info)
  } else if(method == 'exp_soft') {
    hyper.move = ExpHyperMove(network.info, 'soft', GLOBvar, hyper.proposals)
  } else if(method == 'exp_hard') {
    hyper.move = ExpHyperMove(network.info, 'hard', GLOBvar, hyper.proposals)
  } else if(method == 'bino_soft') {
    hyper.move = BinoHyperMove(network.info, 'soft', GLOBvar)
  } else if(method == 'bino_hard') {
    hyper.move = BinoHyperMove(network.info, 'hard', GLOBvar)
  } 
  
  return(hyper.move)
}

