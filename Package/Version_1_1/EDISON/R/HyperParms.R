HyperParms <-
function(options){

  alphaCP = options$alphaCP; betaCP = options$betaCP
  alphaTF = options$alphaTF; betaTF = options$betaTF
  dyn = options$dyn
  smax = options$maxCP; qmax = options$maxTF
  
  #####################################################################
  ### rjMCMC hyperparameters 
  #####################################################################

  ### Level 1 (4 moves: CP birth, CP death, CP update or phase update)

  ## For birth/death/move/update phase acceptance rates
  
  # Unless changepoint sampling is enabled, set proportion of 
  # CP moves proposed to 0
  if(options$cp.fixed) {
    cD = 0 
  } else {
    cD = 0.1
  }
  
  ### level 2 (4 moves)(for each current phase: Pred birth, Pred death or Regression Coefficient update)
  ## for Pred birth/death acceptation
  c = 0.5

  ## For each hidden state (model selection)
  # sig2 ~ IG (v0/2,gamma0/2)
  v0 = 1
  gamma0 = 0.1
  
  # For the signal-to-noise ratio
  # delta2 ~ IG(alphad2,betad2)
  alphad2 = 2
  betad2 = 5
  #######################################################################
  #######################################################################
  
 
  ## For the number of changepoints (CP)
  # for D sampling (D ~ Ga(alphaD,betaD)
  alphaD = alphaCP
  betaD = betaCP
  
  ## For the number of Transcription Factors (TF)
  ## for lambda ~ Ga(alphalbd,betalbd)
  alphalbd = alphaTF
  betalbd = betaTF

  HYPERvar = list(cD=cD, alphaD=alphaD, betaD=betaD, c=c, v0=v0, 
                  gamma0=gamma0, alphad2=alphad2, betad2=betad2, 
                  alphalbd=alphalbd, betalbd=betalbd)
  
  return(HYPERvar)
}

