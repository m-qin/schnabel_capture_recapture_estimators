if (!require("fishmethods")) install.packages("fishmethods")
library(fishmethods)
library(data.table)

data(Gerking)

# use my estimators
gerking_reformatted <- as.data.table(Gerking)
gerking_reformatted[, M := cumsum(nM) - nM]
gerking_reformatted[, d := C - R]
colnames(gerking_reformatted) <- c("sample_size", "n_tags_in_sample", "n_new_tags", "n_tags_in_pond", "n_untagged_in_sample")
gerking_reformatted <- gerking_reformatted[2:nrow(gerking_reformatted), ] # remove the first row, which was the first tagging (0 recaptures by definition)
gerking_results <- data.table(Dataset = "Gerking",
                              Estimator1_with3terms = estimator1(gerking_reformatted, n_terms = 3),
                              Estimator1_with5terms = estimator1(gerking_reformatted, n_terms = 5),
                              Estimator1_with7terms = estimator1(gerking_reformatted, n_terms = 7),
                              Estimator1_Bound1b = estimator1b_bound(gerking_reformatted),
                              Estimator2 = estimator2(gerking_reformatted),
                              Estimator4 = estimator4(gerking_reformatted),
                              Estimator5 = estimator5(gerking_reformatted, lowerbound_log10 = 1, upperbound_log10 = 5),
                              Estimator6 = estimator6(gerking_reformatted, lowerbound_log10 = 1, upperbound_log10 = 5),
                              Estimator7 = estimator7(gerking_reformatted))