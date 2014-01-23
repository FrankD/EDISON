proposeContinuous <-
function(orig_beta, proposal_range, limit=30) {

  a = orig_beta - proposal_range;
  b = orig_beta + proposal_range;

  new_beta = a + (b-a)*runif(1, 0, 1);
 
  if (new_beta > limit)
    new_beta = (2*limit) - new_beta;
 
  return(abs(new_beta))

}

