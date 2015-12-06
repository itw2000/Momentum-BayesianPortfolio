data { 
  int N; #Number of time points
  int m; #Number of assets
  matrix[N,m] y;
  matrix[m,m] P_0_inv;
  real epsilon1;
  real epsilon2;
  vector[m] vec1;
  real kappa_0;
}
parameters {
  vector[m] mu;
  cov_matrix[m] Sigma;
  real mu_0;
  real tau_0_2;
  real<lower=m> nu; 
}
#transformed parameters{
 # matrix[m,m] Sigma_t;
  #matrix[m,m] P_0_inv_t;
  
  #Sigma_t <- Sigma/kappa_0;
  #for (i in 1:m){
  #  for (j in 1:m){
  #    Sigma_t[i,j] <- Sigma[i,j]/kappa_0;
  #  }
  #}
  
  #P_0_inv_t <- P_0_inv/tau_0_2;
  #for (i in 1:m){
  #  for (j in 1:m){
  #    P_0_inv_t[i,j] <- P_0_inv[i,j]/tau_0_2;
  #  }
  #}
  
  
#}
model {
  for (i in 1:N){
    y[i] ~ multi_normal(mu, Sigma);
  }
	  
  mu ~ multi_normal(vec1*mu_0, Sigma/kappa_0);
  
  Sigma ~ inv_wishart(nu, P_0_inv/tau_0_2);

  tau_0_2 ~ inv_gamma(epsilon1,epsilon2);
}