ProposeDiscrete <-
function(params.old, proposal.range, max.range) {
  # Propose a new discrete parameter, based on the previous value, within the
  # given proposal range, making sure that the maximum range is not exceeded. 
  #
  # Args:
  #  params.old:     Old value for the parameter
  #  proposal.range: Range for the proposal
  #  max.range:      Total range for the parameter
  #
  # Returns:
  #  New value for the parameter
  
  extent = (params.old - proposal.range):(params.old + proposal.range)
  
  pick = sample(1:length(extent), 1)
  
  params.new = extent[pick]
  
  if(params.new < 1) { 
    params.new = max.range + params.new
  } else if(params.new > max.range) {
    params.new = params.new - max.range
  }
  
  return(params.new)
}

