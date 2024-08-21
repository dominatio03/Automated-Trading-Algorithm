source('framework/data.R')
source('framework/backtester.R')
source('framework/processResults.R')
source('strategies/Final.R')

# Define analysis parameters
dataList <- getData(directory="PART1+2")
inSampleDays <- 1200
numDays <- nrow(dataList[[1]])
dataList <- lapply(dataList, function(x) x[1:inSampleDays])
#dataList <- lapply(dataList, function(x) x[(inSampleDays+1):numDays])
sMult <- 0.2 # Slippage multiplier

# Ranges for threshold1 and threshold2
threshold1_range <- seq(from=-5000, to=5000, by=1000)
threshold2_range <- seq(from=-5000, to=5000, by=1000)

# Total number of combinations
numberComb <- length(threshold1_range) * length(threshold2_range)

# Initialize results matrix and portfolio PnL list
resultsMatrix <- matrix(nrow=numberComb, ncol=3)
colnames(resultsMatrix) <- c("Threshold 1", "Threshold 2", "PD Ratio")
pfolioPnLList <- vector(mode="list", length=numberComb)

# Loop through combinations
count <- 1
for (threshold1 in threshold1_range) {
  for (threshold2 in threshold2_range) {
    # Define strategy parameters based on "Final" list
    params <- list(
      series = 1:10,
      spreadPercentage = 0.02,
      takeProfitRatio = 0.3,
      threshold1 = c(threshold1, 0, 0, 0, 0, 0, 0, 0, threshold1, 0),
      threshold2 = c(threshold2, 0, 0, 0, 0, 0, 0, 0, threshold2, 0),
      risk_per_trade = c(0.0005, 0, 0, 0, 0, 0, 0, 0, 0.07, 0),
      budget = c(250000, 0, 0, 0, 0, 0, 0, 0, 50000, 0)
    )
    
    # Backtest the strategy
    results <- backtest(dataList, getOrders, params, sMult) # Assuming getOrders from Final.R
    pfolioPnL <- plotResults(dataList, results)
    
    # Store results and print progress
    resultsMatrix[count, ] <- c(threshold1, threshold2, pfolioPnL$fitAgg)
    pfolioPnLList[[count]] <- pfolioPnL
    cat("Just completed", count, "out of", numberComb, "\n")
    print(resultsMatrix[count, ])
    count <- count + 1
  }
}

# Analyze and present results
sortedResults <- resultsMatrix[order(resultsMatrix[, "PD Ratio"], decreasing = TRUE), ]
print(sortedResults)

# Save results (optional)
Finalresults <- data.frame(resultsMatrix)
colnames(Finalresults) <- c("Threshold 1", "Threshold 2", "PD Ratio")
write.csv(Finalresults, "opt_final_part_in_sample threhold.csv")