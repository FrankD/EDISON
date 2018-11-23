#' Calculates the ratio of two likelihoods in a structure move.
#'
#' This function calculates the ratio of the liklihoods in a network structure
#' move. The returned value is the ratio for the modification of one edge in
#' one segment.
#'
#'
#' @param gamma0 Hyperparameter.
#' @param y Target data.
#' @param x.orig The original observation adta
#' @param x.new The edge-modifed observation data.
#' @param v0 Hyperparameter.
#' @param delta2 Delta squared parameter (signal-to-noise).
#' @param dir Direction of the change: 1 = Added an edge. 2 = Removed an edge.
#' 0 = No change.
#' @return Returns the likelihood ratio.
#' @author Frank Dondelinger
#' @seealso \code{\link{CalculatePriorRatio}}
#' @references For more information about the hyperparameters and the
#' functional form of the likelihood, see:
#'
#' Dondelinger et al. (2012), "Non-homogeneous dynamic Bayesian networks with
#' Bayesian regularization for inferring gene regulatory networks with
#' gradually time-varying structure", Machine Learning.
#' @export CalculateLikelihoodRatio
CalculateLikelihoodRatio <-
  function(x.new, x.orig, y, gamma0, delta2, v0, dir) {
    if (dir == 0) {
      return(1)
    } else {
      N <- length(y)
      r.seg <- ((gamma0 + calculateResidual(x.new, y, delta2)) /
                  (gamma0 + calculateResidual(x.orig, y, delta2))) ^ (-(N + v0) / 2)
      return(r.seg * sqrt(1 + delta2) ^ (-dir))
    }
  }
