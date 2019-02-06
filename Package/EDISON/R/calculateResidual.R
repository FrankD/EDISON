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

  L = t(y) %*% x;

  return (t(y) %*% y - (delta2/(delta2+1)) %*% L %*% chol2inv(chol(crossprod(x))) %*% t(L))  
}