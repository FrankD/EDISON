AcceptableMove <-
function(proposal, qmax, self.loops, target) {
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
  
  return(acceptable)
}

