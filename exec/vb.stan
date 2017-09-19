//using uniform dist for uncertainty

//do the linf and lt equation thingy
data {
  int <lower=0> N;
  //length
  real<lower=0> length[N];
  //age
  int<lower=0> age[N];
}

transformed data{
  //expected length at time/age t
  real log_length[N];
  for (i in 1:N){
    log_length[i] = log(length[i]);
  }
}

parameters {
  real<lower=0> asym_length;
  real <lower=0,upper=3> growth_coef;
  real<lower=0> sigmasq;
  real <upper=0>theor_age_length0;
}


transformed parameters {
  real<lower=0> sigma;
  real exp_log_length[N];
  sigma = sqrt(sigmasq);
  for (i in 1:N){
    exp_log_length[i] = log(asym_length)+log(1-exp(-growth_coef*(age[i]-theor_age_length0)));
  }
}

model {
  //sigmasq~inv_gamma(0.001,0.001);
  log_length ~ normal(exp_log_length,sigma);
}
