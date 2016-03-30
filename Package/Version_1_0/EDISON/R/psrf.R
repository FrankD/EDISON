psrf <-
function(parameters) {
  
  # Number of sequences
  nbSeq=length(parameters)
  
  nbIterations=dim(parameters[[1]])[2]
  nbPars = dim(parameters[[1]])[1]
  
  # Compute B
  seq_means = matrix(0, nbPars, nbSeq)
  
  for(i in 1:nbSeq) {
    seq_means[, i] = apply(parameters[[i]], 1, mean)
  }
  
  B = nbIterations/(nbSeq-1)*
    apply((seq_means-matrix(
      apply(seq_means,1,mean),nbPars,nbSeq))^2,1,sum)
  
  # Compute W
  diffs = matrix(0, nbPars, nbSeq)
  
  for(i in 1:nbSeq) {
    diffs[, i] = apply((parameters[[i]] - kronecker(seq_means[, i], matrix(1, 1
                                                                           , nbIterations)))^2, 1, sum)
  }
  
  W = apply(diffs, 1, sum) / (nbSeq*(nbIterations-1))
  
  seq_overest = (nbSeq + 1) / nbSeq;
  
  PSRF = seq_overest*((nbIterations-1)/nbIterations+B/(W * nbIterations)) - 
    (nbIterations - 1)/(nbIterations*nbSeq)
  
  if(any(B == 0)) {
    PSRF[B==0] = seq_overest*((nbIterations-1)/nbIterations) - 
      (nbIterations - 1)/(nbIterations*nbSeq)
  }
    
  return(PSRF)
}

