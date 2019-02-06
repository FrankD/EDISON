#' Sample initial sigma squared.
#' 
#' This function samples the initial values for the sigma squared variance from
#' the inverse gamma prior.
#' 
#' 
#' @param y Target data.
#' @param x Prediction data.
#' @param v0 Inverse gamma prior hyperparameter.
#' @param gamma0 Inverse gamma prior hyperparameter.
#' @param delta2 Hyperparameter.
#' @return The sampled sigma squared values.
#' @author Sophie Lebre
#' @references For more information about the model, see:
#' 
#' Dondelinger et al. (2012), "Non-homogeneous dynamic Bayesian networks with
#' Bayesian regularization for inferring gene regulatory networks with
#' gradually time-varying structure", Machine Learning.
#' @export sampleSig2
sampleSig2 <-
function(y, x, v0, gamma0, delta2) {
  out = rinvgamma(1, shape=v0/2 + length(y)/2, 
                  scale = (gamma0 + calculateResidual(x, y, delta2))/2)
  return(out)
}

