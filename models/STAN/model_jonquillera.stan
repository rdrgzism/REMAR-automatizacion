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

  // priors (metiere specific: Order: Transit  Calada   Recollir
  // displacement (shape and rate for metiere-specific gamma distributions)
  array [2] real prior_shape_1;    //Transit (estimate and precision)
  array [2] real prior_shape_2;    //Calada
  array [2] real prior_shape_3;    //Recollir

  array [2] real prior_rate_1; //Transit
  array [2] real prior_rate_2; //Calada
  array [2] real prior_rate_3; //Recollir

  // angles
  array [2] real prior_x1;     //Transit
  array [2] real prior_x2;     //Calada
  array [2] real prior_x3;     //Recollir
}

transformed data {
}

parameters {
  vector[N] xangle; // von mises
  vector[N] yangle;

  array [N] real <lower=0> shape;   // gamma
  array [N] real <lower=0> rate;

  vector <lower = 0, upper = 1> [5] transition; // number of cells to be estimated from the transition probs matrix (metier-dependent)
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
  // Transit
  trans_mat[1, 1] = transition[1];                    // Staying in transit state
  trans_mat[1, 2] = transition[3];                    // Transit -> Recollir
  trans_mat[1, 3] = 1.0 - transition[1] - transition[3]; // Transit -> Calada

  // Recollida
  trans_mat[2, 1] = transition[2];                    // Recollir -> Transit
  trans_mat[2, 2] = 1.0 - transition[2];              // Recolli -> Recollir
  trans_mat[2, 3] = 0;                                // Recollir -> Calada

  // Calada
  trans_mat[3, 1] = transition[4];                    // Calar -> Transit
  trans_mat[3, 2] = transition[5];                    // Calar -> Recollir
  trans_mat[3, 3] = 1.0 - transition[4] - transition[5]; // Staying in recollir state

  // Calculate statdist (using softmax to ensure valid simplex)
  vector[N] statdist_raw;
 statdist_raw = inverse(diag_matrix(rep_vector(1.0, N)) - trans_mat + rep_matrix(1.0, N, N)) * rep_vector(1.0, N);
  statdist = softmax(statdist_raw); // Ensure statdist is a valid simplex
}

model {
  vector[N] logp;
  vector[N] logp_1;
  array [N] vector[N] log_trans_mat_tr;

  transition ~ beta(1,1);

  // metiere specific
  // Order: Transit     Recollir Calada
  shape[1] ~ normal(prior_shape_1[1], prior_shape_1[2]);    // transit
  shape[2] ~ normal(prior_shape_2[1], prior_shape_2[2]);    // recollir
  shape[3] ~ normal(prior_shape_3[1], prior_shape_3[2]);    // calada

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
  array[T] int z_raw = viterbi(T, N, steps, angles, trans_mat, statdist, shape, rate, loc, kappa);
  array[T] int<lower=1, upper=N> z_rep;
  for (t in 1:T) {
    z_rep[t] = z_raw[t];
  }
}
