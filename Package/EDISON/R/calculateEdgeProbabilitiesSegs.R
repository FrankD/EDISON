#' Calculate edge probabilities for fixed segments.
#' 
#' This function calculates the marginal posterior probabilities for the edges
#' in each network for the specified segments.
#' 
#' 
#' @param prob.networks List containing the marginal posterior probabilities
#' for the edges of each network at each timepoint, from
#' \code{\link{calculateEdgeProbabilitiesTimePoints}}.
#' @param cps Changepoints defining the segments for which the edge
#' probabilities should be calculated. Note that these are global changepoints
#' that apply to the whole network.
#' @param num.targets Number of target nodes in the network.
#' @param num.preds Number of predictors in the dataset.
#' @return Returns a list of length equal to the number of segments, with each
#' entry containing a matrix of size NumNodes by NumNodes which contains the
#' marginal edge probabilities for that segment.
#' @author Frank Dondelinger
#' @seealso \code{\link{calculateEdgeProbabilities}},
#' 
#' \code{\link{calculateEdgeProbabilitiesTimePoints}}
#' @export calculateEdgeProbabilitiesSegs
calculateEdgeProbabilitiesSegs <-
function(prob.networks, cps, num.targets, num.preds) {
  prob.networks.segs = list() 

  for(cp.i in 2:length(cps)) {
    timePoints = cps[cp.i-1]:(cps[cp.i]-1)
    prob.networks.segs[[cp.i-1]] = matrix(0, num.preds, num.targets) 
  
    for(t in timePoints) {
      prob.networks.segs[[cp.i-1]] = prob.networks.segs[[cp.i-1]] +
        prob.networks[[t]] 
    }
  
    prob.networks.segs[[cp.i-1]] = prob.networks.segs[[cp.i-1]]/length(timePoints)
  }
  
  return(prob.networks.segs)
}

