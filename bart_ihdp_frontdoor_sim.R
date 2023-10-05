

library(bartcs)
library(plotrix)

# Set the directory where your CSV files are located
csv_directory <- "IHDP_frontdooor_data_CSVs/"

# Create a vector of file names (adjust the pattern accordingly)
csv_files <- list.files(path = csv_directory, pattern = "*.csv", full.names = TRUE)

# Create an empty list to store the results
results_list <- list()
i = 1
# Loop through each CSV file and run BART analysis
for (csv_file in csv_files) {
  # Read the CSV file
  ihdp <- read.csv(csv_file)
  
  alpha = mean(ihdp$m[ihdp$t== 1])- mean(ihdp$m[ihdp$t== 0])
  fit <- single_bart(
    Y               = ihdp$y,
    trt             = ihdp$m,
    X               = data.frame(ihdp$t),
    num_tree        = 10,
    num_chain       = 4,
    num_post_sample = 100,
    num_burn_in     = 100,
    verbose         = FALSE
  )
  results_list <- c(results_list, alpha * mean(unlist(fit$mcmc_list[, "ATE"])))
  print(i)
  i = i+1
  # if (i == 10) {
  #break
  #}
}

# Now 'results_list' contains the BART analysis results for each CSV file
ATE = 10
mse <- mean((unlist(results_list) - ATE)^2)
merr <- abs(mean(unlist(results_list) - ATE))
print(paste("ATE:", merr, "+-", std.error(unlist(results_list))))