rinvgamma <-
function(n, shape, scale) {
    return(1 / rgamma(n, shape=shape, scale=1/scale))
}

