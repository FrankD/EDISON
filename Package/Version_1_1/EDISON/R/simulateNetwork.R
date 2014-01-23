simulateNetwork <-
function(l=100, min_phase_length=10, k_bar=10, q=10, 
  lambda_2=0.45, noise=0.25, net=NULL, lambda_3=2, spacing=0, 
  gauss_weights=FALSE, same=FALSE, changes='sequential', 
  fixed=FALSE, cps=NULL, saveFile=NULL) {

  if(is.null(net)) {
    net = generateNetwork(lambda_2, q, min_phase_length, k_bar, l, lambda_3,
                           spacing, gauss_weights, same, changes, fixed, cps)
  } else {
    l = net$l
    q = dim(net$network[[1]])[1]
  }
  
  network = net$network; epsilon = net$epsilon;
  k = net$k; changes = net$changes;
  
  # Simulate data from network
  sim_data = matrix(0, q, l) 
  sim_data[,1] = matrix(rnorm(q), q, 1);
  
  matrix_offset = 0; begin = 2;
  
  counter = 0;
  
  for (i in 1:(k+1)) {
	
	  parent_set = network[[i]];

	  change = epsilon[[i]];
	  
	  if(length(noise) == 1) {
	    seg_noise = noise;
	  } else {
	    seg_noise = noise[i];
	  }
	
    # Calculate regression model results for current segment  
    for (j in begin:change) {
	    new_pt = t(parent_set) %*% sim_data[,j-1]; 	
	  
	    # Nodes without parent are drawn from Gaussian
	    new_pt[new_pt == 0] = rnorm(sum(new_pt == 0));
	  
	    # Add noise
	    new_pt = new_pt + matrix(rnorm(q, 0, seg_noise), q, 1);
	  
	    # Scale to preserve variance = 1
	    sim_data[, j] = new_pt / sqrt(1 + seg_noise*seg_noise);
      
      # Fudge factor for correcting for instability  
      fudge = 10;
        
      if(any(abs(sim_data[,j]) > fudge*max(1, seg_noise))) {
        counter = counter + sum(abs(sim_data[,j]) > fudge*max(1, seg_noise));

        new_pt = sim_data[,j]; 
          
        new_pt[abs(new_pt) > fudge*max(1, seg_noise)] =
            new_pt[abs(new_pt) > fudge*max(1, seg_noise)] / fudge;
            
        sim_data[,j] = new_pt;
      }
	  }
	
	  begin = change + 1;
      
  }
  
  network.data = list(sim_data=sim_data, epsilon=epsilon, k=k, network=network, 
                 changes=changes, noise=noise)
  
  if(!is.null(saveFile))
    save(network.data, file=saveFile)
  
  return(network.data);
  
}

