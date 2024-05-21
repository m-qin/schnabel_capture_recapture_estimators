library(purrr)

# I think valid if sample_size << total_fish (t_i << N) because we need fish to be drawn independently
estimator1 <- function(stats, n_terms = 5){
  n_untagged_in_sample <- stats$n_untagged_in_sample
  n_tags_in_pond <- stats$n_tags_in_pond
  n_tags_in_sample <- stats$n_tags_in_sample
  
  most_terms <- sapply(n_terms:1, function(i) sum(n_untagged_in_sample * n_tags_in_pond^i))
  all_terms <- c(most_terms, -sum(n_tags_in_sample))
  all_roots <- polyroot(all_terms)
  real_root <- Re(all_roots)[length(Re(all_roots))]
  return(real_root)
}

# to do: check if I included the N_k = Estimator 1 term correctly
estimator1b_bound <- function(stats, n_terms = 5){
  n_untagged_in_sample <- stats$n_untagged_in_sample
  n_tags_in_pond <- stats$n_tags_in_pond
  n_tags_in_sample <- stats$n_tags_in_sample
  
  last_n_tags_in_pond <- n_tags_in_pond[length(n_tags_in_pond)]
  most_terms <- sapply(n_terms:2, function(i) sum(n_untagged_in_sample * n_tags_in_pond^i) - last_n_tags_in_pond * sum(n_untagged_in_sample * n_tags_in_pond^(i-1)))
  all_terms <- c(most_terms,
                 sum(n_untagged_in_sample * n_tags_in_pond) + last_n_tags_in_pond * sum(n_tags_in_sample),
                 -sum(n_tags_in_sample))
  all_roots <- polyroot(all_terms)
  real_root <- Re(all_roots)[length(Re(all_roots))]
  return(real_root)
}

# today, called "Schnabel method"
# valid if M_i << N (if n_tags_in_pond << total_fish)
estimator2 <- function(stats){
  sum(stats$sample_size * stats$n_tags_in_pond) / sum(stats$n_tags_in_sample)
}

# valid if r_i >> 0 (if n_tags_in_sample >> 0)
estimator4 <- function(stats){
  sqrt(sum(stats$sample_size * stats$n_tags_in_pond) / sum(stats$n_tags_in_sample^2 / stats$sample_size / stats$n_tags_in_pond))
}