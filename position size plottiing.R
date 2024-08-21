library(TTR)
library(quantmod)
library(ggplot2)

# Data loading and preprocessing
data_file <- file.path("DATA", "PART1", "01.csv")
data <- read.csv(data_file)
data$Index <- as.Date(data$Index)
numDays <- nrow(dataList[[1]])

# Convert data to xts object
xts_data <- xts(data[, -1], order.by = data$Index)

# Initialize position_data as a data frame with column names
position_data <- data.frame(
  index = numeric(),
  series1_pos_size = numeric(),
  series9_pos_size = numeric(),
  closing_price = numeric(),
  stringsAsFactors = FALSE
)

# Define your strategy function (e.g., getOrders)
getOrders <- function(store, newRowList, currentPos, info, params) {
  allzero <- rep(0, length(newRowList))
  if (is.null(store)) {
    store <- initStore(newRowList, params$series)
    store$iter <- 1 # Initialize iter if it's not set
  }
  marketOrders <- -currentPos # Initialize market orders as the opposite of current positions
  pos <- allzero
  entry_price <- store$entry_price
  cumulative_return <- vector("numeric", length(params$series))
  lookback <- params$lookback_medium
  stopRate <- 0.3 # Stop-loss rate
  takeProfitRate <- 0.3 # Take-profit rate
  
  cl <- newRowList$Close
  volume <- newRowList$Volume
  high <- newRowList$High
  low <- newRowList$Low
  index <- index(cl) # Get the index (date) for the current series
  
  # Assuming the single series is series 1
  position_size <- ifelse(i == 1, position_size, 0)
  
  # Append position data to the global data frame
  new_row <- data.frame(
    index = index,
    series1_pos_size = position_size,
    series9_pos_size = 0,  # Set series9_pos_size to 0 since there's only one series
    closing_price = cl,
    stringsAsFactors = FALSE
  )
  position_data <<- rbind(position_data, new_row)
  
  # Calculate position size, OBV, and A/D
  # ... (your strategy code for calculating position size, OBV, and A/D)
  
  store$prev_close <- tail(cl, 1)
  store$prev_obv <- tail(obv_values, 1)
  store$prev_ad <- tail(ad_values, 1)
  
  marketOrders <- marketOrders
  store$entry_price <- entry_price
  store$iter <- store$iter + 1 # Increment iter
  return(list(store = store, marketOrders = marketOrders,
              limitOrders1 = allzero, limitPrices1 = allzero,
              limitOrders2 = allzero, limitPrices2 = allzero))
}

# Backtest your strategy
results <- backtest(xts_data, getOrders, params, sMult)

# Plot position size line for series 1
ggplot() +
  geom_line(data = position_data, aes(x = index, y = series1_pos_size, color = "Series 1")) +
  labs(x = "Date", y = "Position Size", color = "Series") +
  theme_bw()