sampleK <-
function(mini, maxi, lambda, nb){
  if( mini == maxi) { 
    print("Error with sampling from a truncated Poisson: min value = max value.") }
  
  out = sample(mini:maxi, nb, replace=TRUE, 
               prob=lambda^(mini:maxi)/apply(matrix(mini:maxi, 1, maxi-mini+1),
                                             2, factorial))
  return(out)
}

