data {
  int N;
  int Y[N];
}

parameters {
  real<lower=0,upper=1> p;
}

model {
  for (n in 1:N)
    Y[n] ~ bernoulli(p);
}
