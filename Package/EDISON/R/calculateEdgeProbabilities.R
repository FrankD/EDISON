#' Calculate the edge probabilities.
#' 
#' This function calculates the marginal posterior probabilities of the edges
#' in the network segments, for each timepoint, and optionally calculates the
#' same for specified changepoints.
#' 
#' 
#' @param network.samples Network samples obtained from the MCMC simulation
#' using \code{\link{EDISON.run}} and \code{\link{runDBN}}.
#' @param cps Optionally specifies changepoints to allow for calculating the
#' marginal posterior edge probabilities for specific segments.
#' @param num.preds Number of predictors in the dataset. If NULL, set to be the same
#' as the number of target variables.
#' @return A list with elements: \item{probs.all}{A list containing marginal
#' edge posterior probabilities for each timepoint.} \item{probs.segs}{A list
#' containing marginal edge posterior probabilities for each specified
#' segment.}
#' @author Frank Dondelinger
#' @seealso \code{\link{calculateEdgeProbabilitiesTimePoints}},
#' 
#' \code{\link{calculateEdgeProbabilitiesSegs}}
#' @examples
#' 
#' # Generate random gene network and simulate data from it
#' dataset = simulateNetwork(l=25)
#' 
#' # Run MCMC simulation to infer networks and changepoint locations
#' result = EDISON.run(dataset$sim_data, num.iter=500)
#' 
#' # Calculate marginal posterior probabilities of edges in the network
#' network = calculateEdgeProbabilities(result)
#' 
#' # Calculate marginal posterior probabilities of edges in the network, 
#' # using the true changepoints
#' true.cps = c(2,dataset$epsilon)
#' network = calculateEdgeProbabilities(result, cps=true.cps)
#' 
#' @export calculateEdgeProbabilities
calculateEdgeProbabilities <-
function(network.samples, cps=NULL, num.preds=NULL) {
  if(is.null(cps)) cps = c(2, network.samples$n+1)
  
  num.targets = length(network.samples) - 1
  
  if(is.null(num.preds)) num.preds = num.targets

  prob.networks = calculateEdgeProbabilitiesTimePoints(network.samples, cps, num.targets, num.preds)
  prob.networks.segs = 
    calculateEdgeProbabilitiesSegs(prob.networks, cps, num.targets, num.preds)
  
  return(list(probs.all=prob.networks, probs.segs=prob.networks.segs))
}

