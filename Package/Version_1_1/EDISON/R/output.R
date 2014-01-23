output <-
function(counters, listStock, GLOBvar, HYPERvar, OUTvar){

  ### Assignment of global variables used here ###
  target = GLOBvar$target
  n = GLOBvar$n
  q = GLOBvar$q
  ### End assignement ###

  ### Assignment of results variables used here ###
  # Counters
  cptMove = counters$cptMove
  acceptMove = counters$acceptMove
  # ListStock
  Estock = listStock$Estock
  Bstock = listStock$Bstock
  hyperstock = listStock$hyperstock
  ### End assignment ###

  ### Assignment of output variables used here ###
  outputFile=OUTvar$outputFile
  analysis = OUTvar$analysis
  ### End assignment ###
 
  results.all = list()

  n_samples = dim(Bstock[[1]])[1]
  after.burnin = round(n_samples/4):n_samples

  # Sample starting from after burnin (default: 1/4 of run)
  if(length(after.burnin) > 1000) {
    sampled = sort(sample(after.burnin, 1000))
  } else {
    sampled = after.burnin
  }

  for(target in 1:q) {
              
    # Collect results
    results = list(cp_samples=Estock[[target]][sampled,], 
      edge_samples = Bstock[[target]][sampled,], 
      target=target, hyper_samples=hyperstock[sampled,], 
      sampled=sampled, counters=counters)
        
    results.all[[target]] = results 
  
    if(OUTvar$by.node && OUTvar$save.file) {
      save(results, file=paste(outputFile, "_analysis_", target, 
	                       sep=""))
    }
  }
      
  if(!OUTvar$by.node && OUTvar$save.file) {
    results = results.all
    save(results, file=paste(outputFile, "_analysis", 
	     sep=""))
	}
	
  results.all$n = n

  return(results.all)
}

