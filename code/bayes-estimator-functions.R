## Explanation of prior parameters for this function

# likelihood: r_i | M_i, p_s ~ Bin(M_i, p_s)
# prior for sampling probability: Let p_s ~ Beta(a, b); by default, use prior Beta(1,1) = Unif(0,1)
# posterior for sampling probability: p_s | r_i, M_i ~ Beta(a + sum(r_i), b + sum(M_i - r_i))

# likelihood: Let N | lambda ~ Pois(lambda), so t_i | lambda, p_s ~ Pois(lambda p_s)
# prior for lambda: lambda p_s ~ Gamma(shape = s, rate = r); by default, use prior s = 1, r = 1
# posterior for lambda: lambda p_s | t_i ~ Gamma(shape = s + sum(t_i), rate = r + n)

# Notice, conditional prior for lambda: lambda | p_s ~ Gamma(s, r p_s)
# Notice, conditional posterior for lambda: lambda | p_s, t_i ~ Gamma(s + sum(t_i), (r + n) p_s)
# Therefore, posterior mean for lambda is E_{p_s}[E_lambda(lambda | p_s, t_i) | r_i, M_i] = E_{p_s}[(s + sum(t_i)) / ((r + n) p_s) | r_i, M_i]
estimator7 <- function(stats, sampling_prob_prior_a = 1, sampling_prob_prior_b = 1, lambda_prior_shape = 1, lambda_prior_rate = 1, n_post_MC_samples = 10^3){
  n_untagged_in_sample <- stats$n_untagged_in_sample # vector of d_i
  n_tags_in_pond <- stats$n_tags_in_pond # vector of M_i
  n_tags_in_sample <- stats$n_tags_in_sample # vector of r_i
  sample_size <- stats$sample_size # vector of t_i
  n_samples <- length(sample_size) # n
  
  # posterior for sampling probability: p_s | r_i, M_i ~ Beta(a + sum(r_i), b + sum(M_i - r_i))
  sampling_prob_post_a <- sampling_prob_prior_a + sum(n_tags_in_sample)
  sampling_prob_post_b <- sampling_prob_prior_b + sum(n_tags_in_pond) - sum(n_tags_in_sample)
  MC_sampling_prob_post <- rbeta(n_post_MC_samples, sampling_prob_post_a, sampling_prob_post_b)
  
  # conditional posterior for lambda: lambda | p_s, t_i ~ Gamma(s + sum(t_i), (r + n) p_s)
  lambda_condtl_post_shape <- lambda_prior_shape + sum(sample_size)
  lambda_condtl_post_rate <- function(sampling_prob) (lambda_prior_rate + n_samples) * sampling_prob
  
  # return posterior mean of lambda using MC
  MC_lambda_post <- lambda_condtl_post_shape / lambda_condtl_post_rate(MC_sampling_prob_post)
  return(mean(MC_lambda_post))
}