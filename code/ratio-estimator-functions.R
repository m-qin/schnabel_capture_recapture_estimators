# Motivation: Assume E[t_i]/N = E[t_i/N] = E[r_i / M_i], so N = E[t_i] / E[r_i / M_i]
estimator8 <- function(stats){
  sum(stats$sample_size) / sum(stats$n_tags_in_sample / stats$n_tags_in_pond)
}

# dangerous; do not use: may divide by r_i=0, and estimator will evaluate to Inf
estimator9 <- function(stats){
  sum(stats$sample_size * stats$n_tags_in_pond / stats$n_tags_in_sample)
}

# dangerous; do not use: may divide by r_i=0, and estimator will evaluate to Inf
estimator10 <- function(stats){
  sum(stats$n_tags_in_pond / stats$n_tags_in_sample) / (sum(1 / stats$sample_size))
}
