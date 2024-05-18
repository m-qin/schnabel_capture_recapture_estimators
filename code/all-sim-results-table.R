library(data.table)

N_SIMS <- 30

all_sim_results <- expand.grid(Dataset = c("I", "II", "III"),
                               SamplingScheme = c("Binomial", "Poisson"),
                               SimID = 1:N_SIMS,
                               Truth = 0,
                               Estimator1_with3terms = 0,
                               Estimator1_with5terms = 0,
                               Estimator1_with7terms = 0,
                               Estimator1_Bound1b = 0,
                               Estimator2 = 0,
                               Estimator4 = 0,
                               Estimator5 = 0,
                               Estimator6 = 0,
                               Estimator7 = 0)
all_sim_results <- as.data.table(all_sim_results)

fill_in_results_table_for_one_sim <- function(dataset, sampling_scheme, sim_id, truth, stats){
  all_sim_results[Dataset == dataset & SamplingScheme == sampling_scheme & SimID == sim_id,
                  `:=`(Truth = truth,
                       Estimator1_with3terms = estimator1(stats, n_terms = 3),
                       Estimator1_with5terms = estimator1(stats, n_terms = 5),
                       Estimator1_with7terms = estimator1(stats, n_terms = 7),
                       Estimator1_Bound1b = estimator1b_bound(stats),
                       Estimator2 = estimator2(stats),
                       Estimator4 = estimator4(stats),
                       Estimator5 = estimator5(stats),
                       Estimator6 = estimator6(stats),
                       Estimator7 = estimator7(stats))]
}