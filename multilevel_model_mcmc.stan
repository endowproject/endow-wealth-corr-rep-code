functions {
  real partial_sum_lpmf(array[] int y_slice,
                         int start, int end,
                         array[] int class_id,
                         vector Z,
                         array[] int ll,
                         array[] vector beta) {
    vector[end - start + 1] X_beta_ll;
    for (n in start:end) {
      X_beta_ll[n - start + 1] = Z[n] * beta[ll[n]][class_id[n]];  // direct index, no dot product
    }
    return poisson_log_lpmf(y_slice | X_beta_ll);
  }
}
data {
  int<lower=1> D;
  int<lower=1> N;
  int<lower=1> L;
  array[N] int y;
  array[N] int<lower=1, upper=L> ll;
  array[N] int<lower=1, upper=D> class_id;   // replaces row_vector[D] X
  vector[N] Z;
  vector[L] gini;
  array[4] int<lower=1, upper=D> EC;
}
parameters {
  vector[D] mu;
  vector<lower=0>[D] sigma;
  array[L] vector[D] beta_raw;
  real alpha_0;
  real alpha_1;
  real<lower=0> sigma_alpha;
}
transformed parameters {
  array[L] vector[D] beta;
  for (l in 1:L) {
    beta[l] = mu + sigma .* beta_raw[l];
  }
}
model {
  sigma ~ cauchy(0, 2.5);
  sigma_alpha ~ cauchy(0, 2.5);
  mu ~ normal(0, 100);
  alpha_0 ~ normal(0, 100);
  alpha_1 ~ normal(0, 100);

  for (l in 1:L) {
    beta_raw[l] ~ std_normal();
  }

  for (l in 1:L) {
    gini[l] ~ normal(
      alpha_0 + alpha_1 * (
        exp(beta[l][EC[1]]) /
        (exp(beta[l][EC[1]]) + exp(beta[l][EC[2]]) + exp(beta[l][EC[3]]) + exp(beta[l][EC[4]]))
      ),
      sigma_alpha
    );
  }

  int grainsize = 1;
  target += reduce_sum(partial_sum_lpmf, y, grainsize, class_id, Z, ll, beta);
}
generated quantities {
  array[L] real ec;
  for (l in 1:L) {
    ec[l] = exp(beta[l][EC[1]]) /
      (exp(beta[l][EC[1]]) + exp(beta[l][EC[2]]) + exp(beta[l][EC[3]]) + exp(beta[l][EC[4]]));
  }
}
