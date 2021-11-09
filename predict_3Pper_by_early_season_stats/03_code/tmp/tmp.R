library(rstan)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())
# generate data

seed = 1031
set.seed(seed)
Y <- rep(c(1, 0), c(20, 36 - 20))
sum(Y)
N <- 36
data <- list(Y=Y,N=N)

fit <- stan(file='tmp_stan.stan', data=data, seed=seed)

print(fit, probs = c(0.025, 0.1, 0.5, 0.9, 0.975))
traceplot(fit, inc_warmup = TRUE)
stan_hist(fit,pars="p")


Y <- rep(1,29)#29連勝
N <- 29
data <- list(Y=Y,N=N)

fit <- stan(file='tmp_stan.stan', data=data, seed=seed)
print(fit)
stan_hist(fit,pars="p") #勝率p事後分布	
