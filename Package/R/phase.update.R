phase.update <-
function(Eall, Sall, Ball, Sig2all, X, Y, GLOBvar, HYPERvar, 
  target) {
  
  E = Eall[[target]]
  # Current number of changepoints
  s = length(E) - 2
  
  ### Assignment of global variables used here ###
  q = GLOBvar$q
  qmax = GLOBvar$qmax
  Mphase = GLOBvar$Mphase
  nbVarMax = GLOBvar$nbVarMax
  smax = GLOBvar$smax
  lmax = GLOBvar$lmax
  method = GLOBvar$method
  small_prop = GLOBvar$small_prop
  self.loops = GLOBvar$self.loops
  ### End assignment ###

  ### Assignment of hyperparameters variables used here ###
  c = HYPERvar$c
  alphalbd = HYPERvar$alphalbd
  betalbd = HYPERvar$betalbd
  alphad2 = HYPERvar$alphad2
  betad2 = HYPERvar$betad2
  v0 = HYPERvar$v0
  gamma0 = HYPERvar$gamma0
  k = HYPERvar$k
  ### End assignment ###

  prior.params = HYPERvar$prior.params
        
  Sig2 = Sig2all[[target]]
    
  ## Observations in the chosen phase
  y = Y[[target]]
  x = X[[target]]

  model = 0
    
  # Group information about network segments    
  network.info = CollectNetworkInfo(Sall, Eall, prior.params, -1, 
                                    target, q, self.loops, k)
    
  # Try hyperparameter move
  hyper.move = HyperparameterMove(method, network.info, GLOBvar, 
                                  HYPERvar$hyper.proposals)
    
  # If no move made, try structure move
  if(!hyper.move$move.made) {
    ## Compute a structure move 
    bduout =  make_structure_move(x, y, Sall[[target]], Ball[[target]], Sig2, q, 
                qmax, network.info, method, Mphase, E, HYPERvar)
			
    Sall[[target]] = bduout$newS
    Ball[[target]] = bduout$newB

  } else {
    # Hyperparameter move
    prior.params = hyper.move$network.info$prior.params
    k = hyper.move$network.info$k
    bduout = list(move=hyper.move$move, accept=hyper.move$accept)
    
  }
  
  # Update the regression coefficient
  for(posPhase in 1:(s+1)) {  
    ## Update Sig2: case MultiVar
    if(nbVarMax >1){
      Sig2 = Sig2all[[target]][posPhase]
      Sig2all[[target]][posPhase] = 
        updateSigMulti(E[posPhase], X[[target]], Y[[target]], E, 
          Sall[[target]], Ball[[target]], Sig2, Mphase, alphad2, betad2, 
                                                   v0, gamma0)
    } #end update Sig2
  } # end update each phase

  ## Update Sig2: case UniVar
  if(nbVarMax == 1){
    for(target in 1:q) {
      Sig2all[[target]] = 
        updateSigSolo(X[[target]], Y[[target]], E, Sall[[target]], 
                            Ball[[target]], Sig2all[[target]], Mphase, alphad2, 
                            betad2, v0, gamma0)
    }
  }
  
  ##  Return all variables
  ## (+ variable move describing the move type  (1= CP birth, 2= CP death, 3= CP shift, 4= Update phases)
  return( list( E=E, Sall=Sall, Ball=Ball, Sig2all=Sig2all, 
                prior.params=prior.params, k=k,
                accept=bduout$accept, move=bduout$move))
}

