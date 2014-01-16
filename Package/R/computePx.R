computePx <-
function(len, x, delta2){
  # INPUT: len, delimiting breakpoints.
  #        x, the observations of X in the corresponding state
  #        delta2.
  # OUTPUT: the projection matrix Px.

  if(prod(dim(x))>0) {
    moins = (delta2/(delta2+1))* x%*%pseudoinverse(t(x)%*%x)%*%t(x)
  } else {
    moins = matrix(0,len,len)
  }
  
  Px = diag(1,len)-moins
  
  return(Px)
}

