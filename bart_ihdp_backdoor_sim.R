#install.packages("bartcs")
# install.packages("plotrix")
# Assuming you've already installed and loaded the 'bartCS' package
library(bartcs)
library(plotrix)

# Set the directory where your CSV files are located
csv_directory <- "IHDP_data_CSVs/"

# Create a vector of file names (adjust the pattern accordingly)
csv_files <- list.files(path = csv_directory, pattern = "*.csv", full.names = TRUE)

# Create an empty list to store the results
results_list <- list()
i = 1
# Loop through each CSV file and run BART analysis
for (csv_file in csv_files) {
  # Read the CSV file
  ihdp <- read.csv(csv_file)
  
  fit <- single_bart(
    Y               = ihdp$y,
    trt             = ihdp$t,
    X               = ihdp[, 3:27],
    num_tree        = 10,
    num_chain       = 4,
    num_post_sample = 100,
    num_burn_in     = 100,
    verbose         = FALSE
  )
  results_list <- c(results_list, mean(unlist(fit$mcmc_list[, "ATE"])))
  print(i)
  i = i+1
'  if (i == 101) {
  break
  }'
}

# Now 'results_list' contains the BART analysis results for each CSV file
ATE = 4
mse <- mean((unlist(results_list) - ATE)^2)
merr <- abs(mean(unlist(results_list) - ATE))
print(paste("ATE:", merr, "+-", std.error(unlist(results_list))))
# ATE: 0.2314642141566 +- 0.0326129116565807"
# mse: 1.116114


