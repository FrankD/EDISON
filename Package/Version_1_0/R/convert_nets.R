convert_nets <-
function(Ball, Eall) {
  B_nets = list()
  
  # Find 'global' segments (set of all target-specific segments)
  segs = c()
  
  for(target in 1:length(Ball)) {
    segs = c(segs, Eall[[target]]);
  }
  
  segs = sort(unique(segs));
  
  # Initialise segment list
  for(segment in 1:(length(segs)-1)) {
    B_nets[[segment]] = matrix(0, dim(Ball[[1]])[2], length(Ball))
  }
  
  # Build up segment list
  for(target in 1:length(Ball)) {
    local_seg = 0;
    for(segment in 1:(length(segs) - 1)) {
      if(segs[segment] %in% Eall[[target]]) {
        local_seg = local_seg + 1;
        B_nets[[segment]][,target] = Ball[[target]][local_seg,] 
      } else {
        B_nets[[segment]][,target] = Ball[[target]][local_seg,]
      }
    }
  }
  
  return(list(B_nets=B_nets, seg=segs))
}

