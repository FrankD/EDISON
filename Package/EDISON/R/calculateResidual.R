#' Calculates the squared residual between the target data and predicted data
#'
#' Wraps the cpp_calculate_residual which is implemented in C++.
#'
#' @param x Observation data.
#' @param y Target data.
#' @param delta2 Delta squared parameter (signal-to-noise).
#' @return Returns the squared prediction residual.
#' @author Di Lu, Gordon McDonald
#' @export calculateResidual
calculateResidual <- function(x, y, delta2) {
  if (!is.matrix(y)) {
    y = as.matrix(y)
  }
  
  return(cpp_calculate_residual(x, y, delta2))
}


Rcpp::cppFunction('
arma::mat cpp_calculate_residual(arma::mat &x, arma::mat &y, double delta2) {
  arma::mat L = y.t() * x;
  arma::mat xcross = x.t() * x;
  xcross.diag() += 1e-7;

  return y.t()*y - (delta2/(delta2 + 1)) * L * inv_sympd(xcross) * L.t();
}', depends = 'RcppArmadillo')
