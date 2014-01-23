AcceptableMove <-
function(proposal, qmax, self.loops, target, fixed.edges) {
  # Checks if the proposed network is valid (does not exceed the maximum 
  # number of parents per node).
  #
  # Args:
  #  proposal: Proposed network (Kxq matrix with K segments and q 
  #            parent sets
  #  qmax:     Maximum number of parents allowed.
  #
  # Returns:
  #  True if no parent set exceeds the maximum number of parents, false
  #  otherwise
  # Check fan-in restriction
  acceptable = all(apply(proposal, 1, sum) <= qmax)
  
  # Check self-loops
  if(!self.loops) {
    acceptable = acceptable && all(proposal[,target] == 0) 
  }
  
  fixed.segs = t(matrix(c(fixed.edges, -1), dim(proposal)[2], dim(proposal)[1]))
  indices.fixed = fixed.segs >= 0
  
  acceptable = acceptable &&
    all(fixed.segs[indices.fixed] == proposal[indices.fixed])
  
  return(acceptable)
}

