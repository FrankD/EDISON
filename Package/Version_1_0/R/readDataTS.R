readDataTS <-
function(data, posI, t0, tf, m, n){
  # data = matrix to read
  # pos = position of interest
  # m = # of repetitions
  # n = # of timepoints
  # input data is order by repetitions
 
  ### WARNING ###
  # When targetData is read: t0 = dyn and tf = n
  # When predData is read: t0 = 0 and tf = n-dyn

  # Sort positions per time (tps1#1 tps1#2 .. tps1#M tps2#1 ... tpsN#M
  posT = c(sapply(1:n,seq,m*n,by=n))
  
  # Not all timepoints are considered if dyn > 0
  # Predictor : from  T0 to TF-dyn
  # Target : from T0+dyn to TF
  posT = posT[(m*t0+1):(m*tf)]

  # Output matrix (positions sorted and truncated)
  Y = data[posI,posT]
  return(Y)
}

