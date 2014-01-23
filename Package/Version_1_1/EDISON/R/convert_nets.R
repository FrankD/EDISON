convert_nets <-
function(Ball, Eall) {
  
  # Find 'global' segments (set of all target-specific segments)
  segs = c()
  
  for(target in 1:length(Ball)) {
    segs = c(segs, Eall[[target]]);
  }
  
  segs = sort(unique(segs));
  
  # Initialise segment list
  B_nets = list()

  mapping = matrix(0, length(Eall), length(segs) - 1)
  
  # Build up segment list
  local.seg = rep(0, length(Ball))
  empty.matrix = matrix(0, dim(Ball[[1]])[2], length(Ball))
  
  for(segment in 1:(length(segs) - 1)) {
      
    seg.net = empty.matrix
      
    for(target in 1:length(Ball)) {
      local.seg.temp = local.seg[target]

      if(segs[segment] == Eall[[target]][local.seg.temp+1]) {
        local.seg.temp = local.seg.temp + 1
        local.seg[target] = local.seg.temp
      } 
        
      seg.net[,target] = Ball[[target]][local.seg.temp,] 
      mapping[target, segment] = local.seg.temp
    }
      
    B_nets[[segment]] = seg.net
  }

  return(list(B_nets=B_nets, seg=segs, mapping=mapping))
}

