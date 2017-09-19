//using uniform dist for uncertainty

//do the linf and lt equation thingy


data {
  int <lower=0> N1; // number of fish older than 1
  int <lower=0> N0; // number of aged 0 fish in quarter 4
  //length
  real length[N1+N0];
  //age
  vector<lower=0> [N1+N0] age;
  // month
  vector<lower=0,upper=1> [N1+N0] toy;
  real<lower=-pi(),upper=pi()> mu_a;
  real<lower=0> sigma_a;
}

transformed data{
  //expected length at time/age t
  real b[N1+N0];
  for (i in 1:(N1+N0)){
    b[i]=log(length[i]);
  }
}

parameters {
  real<lower=0> asym_length;
  real <lower=0,upper=3> growth_coef;
  real<lower=0> sigmasq;
  vector<lower=-pi(),upper=pi()> [N1] toy_rad1;
  vector<lower=-pi(),upper=pi()/2> [N0] toy_rad0;
  real <upper=0>theor_age_length0;
}

transformed parameters {
  real<lower=0> sigma;
  real exp_log_length[N1+N0];
  vector <upper=0> [N1+N0] x;
  vector <lower=0>[N1+N0] age_vm;
  vector<lower=-pi(),upper=pi()> [N1+N0] toy_rad;
  toy_rad=append_row(toy_rad1,toy_rad0);
  sigma = sqrt(sigmasq);
  age_vm= age-(toy_rad+pi())/(2*pi())+toy;
  x= -growth_coef*(age_vm-theor_age_length0);
  for (i in 1:(N1+N0)){
    exp_log_length[i]=log(asym_length)+log(1-exp(x[i]));
  }
}

model {
  sigmasq~inv_gamma(0.001,0.001);
  toy_rad1 ~ von_mises(mu_a, sigma_a);
  toy_rad0 ~ von_mises(mu_a, sigma_a);
  b ~ normal(exp_log_length,sigma);
}
