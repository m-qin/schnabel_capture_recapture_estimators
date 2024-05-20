## Dataset I from Schnabel (1938)

### Sampling Implementation A (Binomial sampling)

# I chose SAMPLE_SIZE = 200 because 15,284 / 79 is approx 200
paper_dataset1A_sim <- function(sim_id, seed = sim_id){
  stats <- binom_sampling(seed = seed, N_FISH = 1300542, N_SAMPLES = 79, SAMPLE_SIZE = 200)
  fill_in_results_table_for_one_sim(dataset = "I", sampling_scheme = "Binomial", sim_id = sim_id, truth = 1300542, stats = stats)
  invisible(NULL)
}

results_not_printed <- sapply(1:N_SIMS, paper_dataset1A_sim)

### Sampling Implementation C (Poisson sampling)

# I chose RATE = 200 because 15,284 / 79 is approx 200
paper_dataset1C_sim <- function(sim_id, seed = sim_id){
  stats <- pois_sampling(seed = seed, N_FISH = 1300542, N_SAMPLES = 79, RATE = 200)
  fill_in_results_table_for_one_sim(dataset = "I", sampling_scheme = "Poisson", sim_id = sim_id, truth = 1300542, stats = stats)
  invisible(NULL)
}

results_not_printed <- sapply(1:N_SIMS, paper_dataset1C_sim)



## Dataset II from Schnabel (1938)

### Sampling Implementation A (Binomial sampling)

# I chose SAMPLE_SIZE = 250 because 10,033 / 39 is approx 250
paper_dataset2A_sim <- function(sim_id, seed = sim_id){
  stats <- binom_sampling(seed = seed, N_FISH = 15344, N_SAMPLES = 39, SAMPLE_SIZE = 250)
  fill_in_results_table_for_one_sim(dataset = "II", sampling_scheme = "Binomial", sim_id = sim_id, truth = 15344, stats = stats)
  invisible(NULL)
}

results_not_printed <- sapply(1:N_SIMS, paper_dataset2A_sim)

### Sampling Implementation C: Poisson sampling

# I chose RATE = 250 because 10,033 / 39 is approx 250
paper_dataset2C_sim <- function(sim_id, seed = sim_id){
  stats <- pois_sampling(seed = seed, N_FISH = 15344, N_SAMPLES = 39, RATE = 250)
  fill_in_results_table_for_one_sim(dataset = "II", sampling_scheme = "Poisson", sim_id = sim_id, truth = 15344, stats = stats)
  invisible(NULL)
}

results_not_printed <- sapply(1:N_SIMS, paper_dataset2C_sim)


## Dataset III from Schnabel (1938)

### Sampling Implementation A (Binomial sampling)

# I chose SAMPLE_SIZE = 150 because 2055 / 14 is approx 150
paper_dataset3A_sim <- function(sim_id, seed = sim_id){
  stats <- binom_sampling(seed = seed, N_FISH = 1872, N_SAMPLES = 14, SAMPLE_SIZE = 150)
  fill_in_results_table_for_one_sim(dataset = "III", sampling_scheme = "Binomial", sim_id = sim_id, truth = 1872, stats = stats)
  invisible(NULL)
}

results_not_printed <- sapply(1:N_SIMS, paper_dataset3A_sim)


### Sampling Implementation C: Poisson sampling

# I chose RATE = 150 because 2055 / 14 is approx 150
paper_dataset3C_sim <- function(sim_id, seed = sim_id){
  stats <- pois_sampling(seed = seed, N_FISH = 1872, N_SAMPLES = 14, RATE = 150)
  fill_in_results_table_for_one_sim(dataset = "III", sampling_scheme = "Poisson", sim_id = sim_id, truth = 1872, stats = stats)
  invisible(NULL)
}

results_not_printed <- sapply(1:N_SIMS, paper_dataset3C_sim)


## Compile results

all_sim_results_long <- melt(all_sim_results,
                             measure = c("Estimator1_with3terms", "Estimator1_with5terms", "Estimator1_with7terms", "Estimator1_Bound1b", "Estimator2", "Estimator4", "Estimator5", "Estimator6", "Estimator7"),
                             variable = "Estimator",
                             value = "Value")
