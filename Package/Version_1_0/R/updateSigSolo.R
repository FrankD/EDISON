updateSigSolo <-
function(X, Y, E, Sall, Ball, Sig2, Mphase, alphad2, betad2, v0, gamma0){
 
  total = 0
  for (phase in E[1:(length(E)-1)]){
    posPhase = which(E == phase)
    S = Sall[posPhase,]
    k = sum(Sall[posPhase,])-1
    #new definition of Mphase: -dyn not required!
    y = Y[(Mphase[phase]:(Mphase[E[posPhase+1]]-1))]
    x = X[(Mphase[phase]:(Mphase[E[posPhase+1]]-1)),]
    delta2 = rinvgamma(1, shape=k + alphad2, scale=betad2 + Ball[posPhase, which(S == 1)] %*% t(x[, which(S == 1)]) %*% x[, which(S == 1)] %*% Ball[posPhase, which(S == 1)] / (2 * Sig2) )
    matPx = computePx(length(y), x[,which(S == 1)], delta2)
    total = total + t(y) %*% matPx %*%y
  }
  
  newSig2=rinvgamma(1, shape=v0/2+length(y)/2, scale = (gamma0+total)/2)
  ## newSig2=rinvgamma(1, shape=v0/2+length(y)/(length(E)-1)/2, scale = (gamma0+total)/(length(E)-1)/2)
  ## newSig2=rinvgamma(1, shape=v0/2+length(y)/2, scale = (gamma0+total/(length(E)-1))/2)
  ## newSig2=rinvgamma(1, shape=v0/2+ mean(E[2:length(E)]-E[1:(length(E)-1)])*m/2, scale = (gamma0+total/(length(E)-1))/2)

  ## mean( rinvgamma(100, shape=v0/2+length(y)/2, scale = (gamma0+total)/2))
  ## var( rinvgamma(100, shape=v0/2+length(y)/2, scale = (gamma0+total)/2))
  ## plot.density(density(rinvgamma(100, shape=v0/2, scale = gamma0/2)))          
  ## mean( rinvgamma(100, shape=v0/2+length(y)/2, scale = (gamma0+total/(length(E)-1))/2))
  ## var( rinvgamma(100, shape=v0/2+length(y)/2, scale = (gamma0+total/(length(E)-1))/2))
  
  return(newSig2)
}

