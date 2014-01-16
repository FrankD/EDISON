defaultOptions <-
function() {
  
  # Maximum number of parent nodes (fan-in restriction). 
  lmax = 5;
  
  # Number of repeated measurements (=1 when no repetition)
  m = 1
  
  # Lag for the DBN model (usually = 1 when X(t) depends on the previous measurement X(t-1), but dyn can be chosen equal to 2, 3, ... )
  dyn = 1 
  
  # Minimal length of a segment (or a phase)
  minPhase = 2
  
  # Maximal number of CPs
  maxCP = 10
  
  # Maximal number of incoming edges (TF) for each node 
  maxTF = 5
  
  # Hyperparameters for the number of CP and incoming edges (TF) 
  alphaCP = 1
  betaCP = 0.5
  alphaTF = 1
  betaTF = 0.5
  
  # By default, do not keep betas constant during burnin
  burnin = FALSE
  
  # Whether to calculate the potential scale reduction factor (PSRF)
  psrf.check = FALSE
  
  # Proposal probabilities for hyperparameter moves
  pp.l2 = 0.01
  pp.l1 = 0.2

  # Output options
  save.by.node = FALSE
  save.file = FALSE

  hyper.fixed = FALSE
  cp.fixed = FALSE

  hyper.init = NULL
  cp.init = NULL

  return(list(lmax=lmax, m=m, dyn=dyn, minPhase=minPhase,
              maxCP=maxCP, maxTF=maxTF, alphaCP=alphaCP, betaCP=betaCP,
              alphaTF=alphaTF, betaTF=betaTF, burnin=burnin,
              psrf.check=psrf.check, save.by.node=save.by.node, save.file=save.file,
              hyper.fixed=hyper.fixed, hyper.init=hyper.init, cp.fixed=cp.fixed,
              cp.init=cp.init, pp.l1=pp.l1, pp.l2=pp.l2))
}

