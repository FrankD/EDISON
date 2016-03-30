dinvgamma <-
function(x, shape, scale=1, log=FALSE) {

    dens = shape * log(scale) - lgamma(shape) - (shape + 1) * log(x) 
            - (scale/x)
  
    if(!log) {
      return(dens)
    } else {
      return(log(dens))
    }
}

