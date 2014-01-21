proposalTuning <-
function(acceptRate, hyper.proposals) {
  if(is.nan(acceptRate[2])) return(hyper.proposals)
  
  decrement = 1 - runif(1, 0, 0.5)
  increment = 1 + runif(1, 0, 0.5)
  
  if(acceptRate[2] < 0.2) {
    hyper.proposals = hyper.proposals*decrement
  } else if(acceptRate[2] > 0.3) {
    hyper.proposals = hyper.proposals*increment
  }

  return(hyper.proposals)
}

