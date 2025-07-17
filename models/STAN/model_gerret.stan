functions {

 // viterbi algorithm for finding most likely sequence of latent states given y
  array[] int viterbi(
  int T,
  int N,
  vector steps,
  vector angles,
  matrix trans_mat,
  vector statdist,
  array [] real shape,
  array [] real rate,
  array [] real loc,
  array [] real kappa) {
 
    array[T] int z_rep;   // simulated latent variables
    
    // the log probability of the best sequence to state k at time n
    array[T, N] real best_lp = rep_array(negative_infinity(), T, 3);   
    
    // the state preceding the current state in the best path
    array[T, N] int back_ptr;
    
    real lp;
    
    // first observation
    for (k in 1:N)
      best_lp[1, k] = log(statdist[k]) +
        gamma_lpdf(steps[1] | shape[k], rate[k]) +
        von_mises_lpdf(angles[1]| loc[k], kappa[k]);
 
    // for each timepoint n and each state k, find most likely previous state j
    for (n in 2:T) {
      for (k in 1:N) {
        for (j in 1:N) {
      // calculate the log probability of path to k from j
          lp = best_lp[n-1, j] + log(trans_mat[j, k]) +
          gamma_lpdf(steps[n] | shape[k], rate[k]) +
          von_mises_lpdf(angles[n]| loc[k], kappa[k]);
    
          if (lp > best_lp[n, k]) {
            back_ptr[n, k] = j;
            best_lp[n, k] = lp;
          }
        }
      }
    }
 
    // reconstruct most likely path
    for (k in 1:N)
      if (best_lp[T, k] == max(best_lp[T]))
        z_rep[T] = k;
    for (t in 1:(T - 1))
      z_rep[T - t] = back_ptr[T - t + 1, z_rep[T - t + 1]];
 
    return z_rep;
  }
  
}

data {
  int<lower=0> T;                 // number of data points
  int<lower=0> N;                 // number of states
  vector [T] steps;               // step length (m)
  vector [T] angles;              // step angle (radians)
  //real dt;                        // time step
  
  // priors (metiere specific: Order: Transit  Cercar   Recollir
  // displacement (shape and rate for metiere-specific gamma distributions)
  array [2] real prior_shape_1;    //Transit (estimate and precision)
  array [2] real prior_shape_2;    //Cercar
  array [2] real prior_shape_3;    //Recollir

  array [2] real prior_rate_1; //Transit
  array [2] real prior_rate_2; //Cercar
  array [2] real prior_rate_3; //Recollir
  
  // angles
  array [2] real prior_x1;     //Transit
  array [2] real prior_x2;     //Cercar
  array [2] real prior_x3;     //Recollir
}

transformed data {
}

parameters {
  vector[N] xangle; // von mises
  vector[N] yangle;
  
  array [N] real <lower=0> shape;   // gamma
  array [N] real <lower=0> rate;
  
  vector <lower = 0, upper = 1> [4] transition; // number of cells to be estimated from the transition probs matrix (metier-dependent)
}

transformed parameters {
  array [N] real <lower=-pi(),upper=pi()> loc;   //angle (von misses)
  array [N] real <lower=0> kappa;                //angle (von misses)
  matrix[N, N] trans_mat; 
  simplex [N] statdist; //steady state (proportions at each state)
  
  
  // derive turning angle mean and concentration (von misses distrubution for angle)
  for(n in 1:N) {
    loc[n] = atan2(yangle[n], xangle[n]);
    kappa[n] = sqrt(xangle[n]^2 + yangle[n]^2);
  }

  // Transition matrix (metier dependent)
  // Order: Transit  Cercar   Recollir
  // Trasit
  trans_mat[1, 1] = transition[1];                    // mantaining at transit state
  trans_mat[1, 2] = 1.0-transition[1];                // transit -> cercar
  trans_mat[1, 3] = 0.0;                              // transit -> recollir
  // Cercar
  trans_mat[2, 1] = transition[2];                    // cercar  -> transit
  trans_mat[2, 2] = transition[3];                    // mantaining at cercar state
  trans_mat[2, 3] = 1.0-transition[2]-transition[3];  // cercar -> recollir
  // Recollir
  trans_mat[3, 1] = 0.0;                              // recollir -> transit
  trans_mat[3, 2] = transition[4];                    // recollir -> cercar
  trans_mat[3, 3] = 1.0-transition[4];                // mantaining at recollir

  statdist = to_vector(rep_row_vector(1, N) / 
             (diag_matrix(rep_vector(1, N)) - 
             trans_mat + rep_matrix(1, N, N)));
}

model {

  vector[N] logp;
  vector[N] logp_1;
  array [N] vector[N] log_trans_mat_tr;
  
  transition ~ beta(1,1);
  
  // metiere specific
  // Order: Transit  Cercar   Recollir
  shape[1] ~ normal(prior_shape_1[1], prior_shape_1[2]);    // transit
  shape[2] ~ normal(prior_shape_2[1], prior_shape_2[2]);    // cercar
  shape[3] ~ normal(prior_shape_3[1], prior_shape_3[2]);    // recollir
  
  rate[1] ~ normal(prior_rate_1[1], prior_rate_1[2]);
  rate[2] ~ normal(prior_rate_2[1], prior_rate_2[2]);
  rate[3] ~ normal(prior_rate_3[1], prior_rate_3[2]);
  
  xangle[1] ~ normal(prior_x1[1], prior_x1[2]);
  xangle[2] ~ normal(prior_x2[1], prior_x2[2]);
  xangle[3] ~ normal(prior_x3[1], prior_x3[2]);
  
  yangle ~ normal(0, 0.5); // zero if mean angle is 0 or pi

  // transpose the tpm and take natural log of entries
  for(i in 1:N){
    for(j in 1:N){
      log_trans_mat_tr[j, i] = log(trans_mat[i, j]);
    }
  }
  
  // forward algorithm implementation
  for(n in 1:N){ // first observation (looping over states)
    logp[n] = log(statdist[n]) + 
      gamma_lpdf(steps[1] | shape[n], rate[n]) +
      von_mises_lpdf(angles[1]| loc[n], kappa[n]);
  }
    
  for (t in 2:T) { // looping over observations
    for (n in 1:N){ // looping over states
      logp_1[n] = log_sum_exp(log_trans_mat_tr[n] + logp) +
        gamma_lpdf(steps[t] | shape[n], rate[n]) +
        von_mises_lpdf(angles[t]| loc[n], kappa[n]);
    }
    logp = logp_1;
  }
  target += log_sum_exp(logp);
}

generated quantities {
  // Viterbi algorithm
  array[T] int<lower=1, upper=N> z_rep;     // simulated latent variables
  //vector<lower=0>[N] y_rep;               // simulated data points
  
  z_rep = viterbi(T, N, steps, angles, trans_mat, statdist, shape, rate, loc, kappa);
  //for (n in 1:T){
  //  y_rep[n] = lognormal_mix_rng(z_rep[n], mu, sigma, y_max);
  //}

}
