CalculateLikelihoodRatio <-
function(gamma0, y, Pxlm, Pxl, v0, delta2, dir) {
  # Calculate the ratio of the likelihood for a structure move. The returned
  # value is the ratio for the modification of one edge in one segment.
  #
  # Args:
  #   gamma0: Hyperparameter
  #   y: Target data
  #   Pxlm: Projection matrix with modified edge
  #   Pxl: Projection matrix
  #   v0: Hyperparameter
  #   delta2: Current value of delta2 parameter.
  #   dir: Direction of the change (1: Add an edge, -1: Remove an edge, 0: Do nothing)
  #
  # Returns:
  #   Ratio of the likelihoods (for one segment).
  
  r.seg = ((gamma0 + t(y) %*% Pxlm %*% y)/(gamma0 + t(y) %*% Pxl %*% y)) ^
            (-(length(y) + v0)/2)
  
  if(dir != 0) {
    r.seg = r.seg * sqrt(1 + delta2)^(-dir)
  } else {
    r.seg = 1 
  }
          
  return(r.seg)

}

