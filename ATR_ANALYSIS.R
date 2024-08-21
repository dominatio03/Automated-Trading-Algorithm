source('framework/data.R')

dataList <- getData(directory="PART1")




library(TTR)

# Assuming dataList[[4]] and dataList[[5]] contain the necessary OHLC (Open, High, Low, Close) data
# Calculate ATR for series 4 and 5 with a commonly used period, such as 14 days
atrPeriod <- 14

atrSeries4 <- ATR(HLC = dataList[[4]][, c("High", "Low", "Close")], n = 14)$atr
atrSeries5 <- ATR(HLC = dataList[[5]][, c("High", "Low", "Close")], n = 14)$atr
atrSeries2 <- ATR(HLC = dataList[[2]][, c("High", "Low", "Close")], n = 14)$atr
atrSeries9 <- ATR(HLC = dataList[[9]][, c("High", "Low", "Close")], n = 14)$atr
atrSeries1 <- ATR(HLC = dataList[[1]][, c("High", "Low", "Close")], n = 14)$atr
atrSeries8 <- ATR(HLC = dataList[[8]][, c("High", "Low", "Close")], n = 14)$atr
atrSeries6 <- ATR(HLC = dataList[[6]][, c("High", "Low", "Close")], n = 14)$atr




# Analyze ATR for series 1
atrMinSeries6 <- min(atrSeries6, na.rm = TRUE)
atrMaxSeries6 <- max(atrSeries6, na.rm = TRUE)

# Optional: Percentiles for series 1
percentile10Series6 <- quantile(atrSeries6, 0.1, na.rm = TRUE)
percentile90Series6 <- quantile(atrSeries6, 0.9, na.rm = TRUE)

# Analyze ATR for series 2
atrMinSeries8 <- min(atrSeries8, na.rm = TRUE)
atrMaxSeries8 <- max(atrSeries8, na.rm = TRUE)

# Optional: Percentiles for series 2
percentile10Series8 <- quantile(atrSeries8, 0.1, na.rm = TRUE)
percentile90Series8 <- quantile(atrSeries8, 0.9, na.rm = TRUE)




# Create a data frame to hold the ATR analysis results
atrAnalysisResults <- data.frame(
  Series = c("Series 6", "Series 8"),
  ATR_Min = c(atrMinSeries6, atrMinSeries8),
  ATR_Max = c(atrMaxSeries6, atrMaxSeries8),
  Percentile_10 = c(percentile10Series6, percentile10Series8),
  Percentile_90 = c(percentile90Series6, percentile90Series8)
)

# Print the data frame to view the results
print(atrAnalysisResults)
