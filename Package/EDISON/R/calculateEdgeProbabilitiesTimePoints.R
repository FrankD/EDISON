#' Calculate the edge posterior probabilities for each timepoint.
#' 
#' This function calculates the marginal posterior edge probabilities of the
#' network at each timepoint.
#' 
#' 
#' @param network.samples Collection of network and changepoint samples of the
#' MCMC simulation, as obtained by \code{\link{EDISON.run}},
#' \code{\link{runDBN}}.
#' @param cps Changepoint vector.
#' @param numNodes Number of nodes in the network.
#' @return A list of length equal to the number of timepoints, where each entry
#' contains a matrix of size NumNodes by NumNodes with the marginal posterior
#' edge probabilities of the network at this timepoint.
#' @author Frank Dondelinger
#' @seealso \code{\link{calculateEdgeProbabilities}},
#' 
#' \code{\link{calculateEdgeProbabilitiesSegs}}
#' @export calculateEdgeProbabilitiesTimePoints
calculateEdgeProbabilitiesTimePoints <- 
  function(network.samples, cps, numNodes) {

  edge_counts <- lapply(seq_len(numNodes), function(i) {

    cp_samples <- network.samples[[i]]$cp_samples
    edge_samples <- network.samples[[i]]$edge_samples
    
    # Expand the <1000 x k> cp_samples to <1000 x n> matrix where each row indicates the changepoint
    # segement a timepoint belongs to 
    timepoint_samples <- apply(cp_samples, 1, function(row) {
      seg.range <- diff(row[row > 0])
      c(1, rep(seq_along(seg.range), seg.range))
    })
    
    # Expand the <1000 x (q+1)*(k-1)> edge_samples to <1000 x q x n> matrix where each sub-matrix is
    # the edge coefficients between i and j \in q at each t \in n.
    edge_coeffs <- lapply(seq_len(nrow(edge_samples)),
                          function(j) matrix(edge_samples[j,], numNodes + 1)[1:numNodes, timepoint_samples[,j]])
    
    # Sum the number of non-zero coefficients, i.e., number of edges.
    sumNonZerosMatrices(edge_coeffs)

  })

  # Get probabilities from counts.
  prob.networks <- edge_counts %>%
    simplify2array(.) %>%
    divide_by(length(network.samples[[1]]$sampled)) %>%
    aperm(., c(2, 1, 3))
  
  return(prob.networks)
}


Rcpp::cppFunction('
NumericMatrix sumNonZerosMatrices(List data) {
  NumericMatrix tmp = as<NumericMatrix>(data[1]);
  int nrow = tmp.nrow();
  int ncol = tmp.ncol();
  
  NumericMatrix ret(nrow, ncol);
  for (int i = 0; i < data.size(); ++i) {
    
    NumericMatrix mat = as<NumericMatrix>(data[i]);
    
    for (int j = 0; j < nrow; ++j) {
      for (int k = 0; k < ncol; ++k) {
        ret(j, k) += ((mat(j, k) != 0) * 1);
      }
    }
    
  }
  
  return ret;
}')