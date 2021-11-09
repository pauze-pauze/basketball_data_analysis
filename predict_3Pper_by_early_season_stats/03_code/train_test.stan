data {
  int N;
  int Y[N];
  real prior_mean;
  real prior_sd;
}

// parameters {
//   real<lower=0.306193673602264,upper=0.406193673602264> mu;
//   real<lower=0.0375059012940355 * 0.9 ,upper=0.0375059012940355 * 1.1> std ;
// }

parameters {
  real<lower=0.2,upper=0.5> p;
}


// model {
//   for (n in 1:N)
//     Y[n] ~ normal(mu, std);
// }

model {
  p ~ normal(prior_mean, prior_sd);
  for (n in 1:N)
    Y[n] ~ bernoulli(p);
}
