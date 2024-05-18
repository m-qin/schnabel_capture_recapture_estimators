estimator5 <- function(stats, lowerbound_log10 = 3, upperbound_log10 = 9){
  restricted_possibilities <- estimator5_log_lik_values(stats, lowerbound_log10, upperbound_log10)
  total_fish_restricted_possibilities <- restricted_possibilities$total_fish_restricted_possibilities
  log_lik_for_restricted_possibilities <- restricted_possibilities$log_lik_for_restricted_possibilities
  estimator5 <- total_fish_restricted_possibilities[which.max(log_lik_for_restricted_possibilities)]
  return(estimator5)
}

hgeom_log_lik_for_one_sample <- function(total_fish, n_tags_in_pond, sample_size, n_untagged_in_sample){
  lfactorial(total_fish - n_tags_in_pond) + lfactorial(total_fish - sample_size) - lfactorial(total_fish - n_tags_in_pond - n_untagged_in_sample) - lfactorial(total_fish)
}

hgeom_log_lik_for_all_samples <- function(total_fish, n_tags_in_pond, sample_size, n_untagged_in_sample){
  log_lik_per_sample <- pmap(list(total_fish, n_tags_in_pond, sample_size, n_untagged_in_sample),
                             hgeom_log_lik_for_one_sample)
  return(sum(unlist(log_lik_per_sample)))
}

estimator5_log_lik_values <- function(stats, lowerbound_log10, upperbound_log10){
  total_fish_possibilities <- round(10^seq(lowerbound_log10, upperbound_log10, length.out = 100))
  total_fish_possibilities <- total_fish_possibilities[total_fish_possibilities >= stats$n_tags_in_pond[length(stats$n_tags_in_pond)]] # only test possible values of total_fish
  log_lik_for_all_possibilities <- sapply(total_fish_possibilities,
                                          hgeom_log_lik_for_all_samples,
                                          n_tags_in_pond = stats$n_tags_in_pond,
                                          sample_size = stats$sample_size,
                                          n_untagged_in_sample = stats$n_untagged_in_sample)
  
  total_fish_restricted_possibilities <- unique(seq(total_fish_possibilities[which.max(log_lik_for_all_possibilities) - 2],
                                                    total_fish_possibilities[which.max(log_lik_for_all_possibilities) + 2],
                                                    length.out = 100))
  log_lik_for_restricted_possibilities <- sapply(total_fish_restricted_possibilities,
                                                 hgeom_log_lik_for_all_samples,
                                                 n_tags_in_pond = stats$n_tags_in_pond,
                                                 sample_size = stats$sample_size,
                                                 n_untagged_in_sample = stats$n_untagged_in_sample)
  return(list(total_fish_restricted_possibilities = total_fish_restricted_possibilities,
              log_lik_for_restricted_possibilities = log_lik_for_restricted_possibilities))
}

plot_estimator5 <- function(stats){
  plot(estimator5_log_lik_values(stats)$total_fish_restricted_possibilities,
       estimator5_log_lik_values(stats)$log_lik_for_restricted_possibilities,
       type = "l",
       ylab = "(Hypergeometric) Log likelihood",
       xlab = "Total fish in the lake")
}

binom_score <- function(total_fish, stats){
  n_untagged_in_sample <- stats$n_untagged_in_sample # d_i in the paper
  n_tags_in_pond <- stats$n_tags_in_pond # M_i in the paper
  n_tags_in_sample <- stats$n_tags_in_sample # r_i in the paper
  
  RHS <- sum(n_tags_in_sample)
  LHS <- sum(n_untagged_in_sample * n_tags_in_pond / (total_fish - n_tags_in_pond))
  
  return(LHS - RHS)
}

estimator6 <- function(stats, lowerbound_log10 = 3, upperbound_log10 = 9){
  total_fish_possibilities <- round(10^seq(lowerbound_log10, upperbound_log10, length.out = 100))
  total_fish_possibilities <- total_fish_possibilities[total_fish_possibilities >= stats$n_tags_in_pond[length(stats$n_tags_in_pond)]] # only test possible values of total_fish
  score_for_all_possibilities <- sapply(total_fish_possibilities, binom_score, stats = stats)
  
  # find the value of total_fish_possibilities that has score closest to 0, then try a finer search
  total_fish_restricted_possibilities <- unique(seq(total_fish_possibilities[which.min(abs(score_for_all_possibilities)) - 2],
                                                    total_fish_possibilities[which.min(abs(score_for_all_possibilities)) + 2],
                                                    length.out = 100))
  score_for_restricted_possibilities <- sapply(total_fish_restricted_possibilities, binom_score, stats = stats)
  
  # find the value of total_fish_possibilities that has score closest to 0
  return(total_fish_restricted_possibilities[which.min(abs(score_for_restricted_possibilities))])
}