EDISON.run <-
function(input, output.file="EDISON.output", 
                       information.sharing='poisson', num.iter=10000, 
                       prior.params=NULL, options=NULL) {

  if(!is.matrix(input)) {
    test = NULL  
    load(file=input)
    data=test$sim_data;
  } else {
    data = input
  }
  
  # Time series length
  n = dim(data)[2]

  # Number of variables
  q = dim(data)[1]

  if(is.null(options)) options = defaultOptions()
  
  ##### Important remark ####
  # if you know the changepoint position, and you want to run the procedure only for estimating the model within phases, 
  # you can  set the variable 'CPinit' to the known CP position  set the variable 'cD=0' in the file 'hyperparms.R' 
  # then CP move will never be considered (only the 4th move 'phase.update' will be considered).
  # however there is still some probleme with the 'output' fonctions in this case, I can help updating this when I will be back to work. 

  # Run TVDBN procedure:
  results = runDBN(targetdata=data, n=n, q=q, niter=num.iter, 
                   method=information.sharing, prior.params=prior.params, 
                   options=options, outputFile=output.file)

  return(results)
}

