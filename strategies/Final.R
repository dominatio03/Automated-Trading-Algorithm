library(TTR)
# Load the TTR (Technical Trading Rules) package for technical analysis functions

library(quantmod)
# Load the quantmod package for quantitative financial modeling
maxRows<-3000
initStore <- function(newRowList, series) {
  # Function to initialize the store object with various data structures and parameters
  store <- list(
    iter = 0, # Initialize iteration counter
    data = newRowList, # Store the input data
    prevMacd = rep(NA, length(series)), # Initialize previous MACD values
    prevSignal = rep(NA, length(series)), # Initialize previous signal values
    prevAdx = rep(NA, length(series)), # Initialize previous ADX values
    entry = rep(NA, length(series)), # Initialize entry prices
    trades = list(), # Initialize list to store trades
    garchPositionSizes = list(), # Initialize list to store GARCH position sizes
    obv_list = vector("list", length(series)), # Initialize list to store OBV values
    prev_close_list = vector("list", length(series)), # Initialize list to store previous closing prices
    prev_obv_list = vector("list", length(series)), # Initialize list to store previous OBV values
    prev_ad_list = vector("list", length(series)), # Initialize list to store previous A/D values
    entry_price = vector("numeric", length(series)), # Initialize vector to store entry prices
    budget = c(250000, 250000, 0, 400000, 200000, 0, 100000, 0, 50000, 250000), # Initial budget for each series
    entryPrice = rep(NA, length(series)), # Initialize vector to store entry prices
    peakPrice = rep(NA, length(series)), # Initialize vector to store peak prices
    troughPrice = rep(NA, length(series)), # Initialize vector to store trough prices
    atr = rep(NA, length(series)), # Initialize vector to store ATR values
    prevZscore = rep(NA, length(series)), # Initialize vector to store previous Z-scores
    Trades = lapply(1:length(newRowList), function(x) list(direction = NULL, entryPrice = NULL)) # Initialize list to store trades
  )
  
  # Initialize OBV, previous closing price, previous OBV, and previous A/D for selected series
  for (i in c(1, 2, 3, 6, 8, 9, 10)) {
    store$obv_list[[i]] <- rep(0, length(newRowList[[series[i]]]$Close))
    store$prev_close_list[[i]] <- 0
    store$prev_obv_list[[i]] <- 0
    store$prev_ad_list[[i]] <- 0
    store$entry_price[i] <- 0 # Initialize with 0
  }
  
  return(store)
}

updateHiStore <- function(HiStore, newRowList, series, iter) {
  # Function to update the high price store
  for (i in 1:length(series))
    HiStore[iter,i] <- as.numeric(newRowList[[series[i]]]$High)
  return(HiStore)
}

updateLoStore <- function(LoStore, newRowList, series, iter) {
  # Function to update the low price store
  for (i in 1:length(series))
    LoStore[iter,i] <- as.numeric(newRowList[[series[i]]]$Low)
  return(LoStore)
}

updateVoStore <- function(VoStore, newRowList, series, iter) {
  # Function to update the volume store
  for (i in 1:length(series))
    VoStore[iter,i] <- as.numeric(newRowList[[series[i]]]$Volume)
  return(VoStore)
}

updateClStore <- function(clStore, newRowList, series, iter) {
  # Function to update the closing price store
  for (i in 1:length(series))
    clStore[iter,i] <- as.numeric(newRowList[[series[i]]]$Close)
  return(clStore)
}

updateStore <- function(store, newRowList, series) {
  # Function to update the store with new data
  store$iter <- store$iter + 1 # Increment iteration counter
  store$h <- updateHiStore(store$h,newRowList,series,store$iter) # Update high price store
  store$l <- updateLoStore(store$l,newRowList,series,store$iter) # Update low price store
  store$cl <- updateClStore(store$cl,newRowList,series,store$iter) # Update closing price store
  store$v <- updateVoStore(store$v,newRowList,series,store$iter) # Update volume store
  store$entry <- store$entry # Update entry prices
  return(store)
}

calculateOBV <- function(close, volume, prev_close, prev_obv) {
  # Function to calculate On-Balance Volume (OBV)
  obv_values <- numeric(length(close))
  prev_obv <- xts(prev_obv, order.by = index(close))
  
  for (i in 1:length(close)) {
    stock_close <- close[i]
    stock_prev_close <- prev_close[[i]]
    stock_prev_obv <- prev_obv[, i]
    
    obv <- ifelse(stock_close > stock_prev_close, stock_prev_obv + volume[i],
                  ifelse(stock_close < stock_prev_close, stock_prev_obv - volume[i], stock_prev_obv))
    
    obv_values[i] <- ifelse(!any(is.na(obv)), tail(obv, 1), 0)
  }
  
  return(obv_values)
}

# Function to initialize the store for A/D
initStoreAD <- function(newRowList, series) {
  ad_list <- vector("list", length(series))
  prev_ad_list <- vector("list", length(series))
  
  for (i in 1:length(series)) {
    ad_list[[i]] <- rep(0, length(newRowList[[series[i]]]$Close))
    prev_ad_list[[i]] <- 0
  }
  
  return(list(ad = ad_list, prev_ad = prev_ad_list))
}

# Function to calculate Accumulation/Distribution (A/D)
calculateAD <- function(close, high, low, volume, prev_ad) {
  ad_values <- numeric(length(close))
  prev_ad <- xts(prev_ad, order.by = index(close))
  
  for (i in 1:length(close)) {
    ad <- ((close[i] - low[i]) - (high[i] - close[i])) / (high[i] - low[i]) * volume[i] + prev_ad[i]
    ad_values[i] <- tail(ad, 1)
  }
  
  return(ad_values)
}

exitOnProfit <- function(currentPos, cl, entry_price, cumulative_return, stock_idx, takeProfitRate = 0.7) {
  # Function to exit a position when a certain profit target is reached
  new_pos <- currentPos
  if (currentPos[stock_idx] != 0) {
    daily_return <- (cl - entry_price[stock_idx]) / entry_price[stock_idx]
    cumulative_return[stock_idx] <- cumulative_return[stock_idx] + daily_return * currentPos[stock_idx]
    if (!is.na(cumulative_return[stock_idx]) && cumulative_return[stock_idx] >= takeProfitRate) {
      new_pos[stock_idx] <- -currentPos[stock_idx]
      cumulative_return[stock_idx] <- 0 # Reset cumulative return after exiting position
    }
  }
  return(list(new_pos = new_pos, cumulative_return = cumulative_return))
}

getOrders <- function(store, newRowList, currentPos, info, params) {
  allzero <- rep(0, length(newRowList))
  
  if (is.null(store)) {
    store <- initStore(newRowList, params$series)
    store$iter <- 1  # Initialize iter if it's not set
  }
  
  marketOrders <- -currentPos  # Initialize market orders as the opposite of current positions
  pos <- allzero
  entry_price <- store$entry_price
  cumulative_return <- vector("numeric", length(params$series))
  lookback <- params$lookback_medium
  stopRate <- 0.3  # Stop-loss rate
  takeProfitRate <- 0.3  # Take-profit rate
  
  ad_obv_data <- data.frame()  # Initialize an empty data frame
  
  for (i in c(1, 9)) {
    stock_idx <- params$series[i]
    cl <- newRowList[[stock_idx]]$Close
    volume <- newRowList[[stock_idx]]$Volume
    high <- newRowList[[stock_idx]]$High
    low <- newRowList[[stock_idx]]$Low
    index <- index(cl)  # Get the index (date) for the current series
    
    # Use risk_per_trade based on series (1, 2, or 9)
    risk_per_trade <- ifelse(i == 1, params$risk_per_trade[1], params$risk_per_trade[9])
    
    position_size <- (risk_per_trade * store$budget[i]) / (stopRate * cl)  # Calculate position size
    price <- cl * position_size
    
    obv_values <- calculateOBV(cl, volume, store$prev_close[[i]], store$prev_obv[[i]])  # Calculate OBV
    ad_values <- calculateAD(cl, high, low, volume, store$prev_ad[[i]])  # Calculate A/D
    
    if (!any(is.na(obv_values)) && !any(is.na(ad_values))) {
      threshold1 <- params$threshold1[i]
      threshold2 <- params$threshold2[i]
      
      if (obv_values > threshold1 && ad_values > threshold1) {
        if (position_size < store$budget[i] && store$budget[i] > 1000 && info$balance > 100000) {
          if (i == 9) {  # Buy for series 2
            marketOrders[i] <- -position_size
            store$budget[i] <- (info$balance * 0.05) - price
          } else {  # Sell for series 9 (opposite of series 2)
            marketOrders[i] <- position_size
            store$budget[i] <- (info$balance * 0.25) - price
          }
          entry_price[i] <- cl
          cumulative_return[i] <- 0
        }
      } else if (obv_values < threshold2 && ad_values < threshold2) {
        if (position_size < store$budget[i] && store$budget[i] > 1000 && info$balance > 100000) {
          if (i == 9) {  # Sell for series 2
            marketOrders[i] <- position_size
            store$budget[i] <- (info$balance * 0.05) - price
          } else {  # Buy for series 9 (opposite of series 2)
            marketOrders[i] <- -position_size
            store$budget[i] <- (info$balance * 0.25) - price
          }
          store$budget[i] <- store$budget[i] - price
          entry_price[i] <- cl
          cumulative_return[i] <- 0
        }
      }
    }
    
    store$prev_close[[i]] <- tail(cl, 1)
    store$prev_obv[[i]] <- tail(obv_values, 1)
    store$prev_ad[[i]] <- tail(ad_values, 1)
    
    # Create a data frame with Series, Date, AD, and OBV values
    ad_obv_row <- data.frame(
      Series = stock_idx,
      Date = index[length(index)],
      AD = ad_values[length(ad_values)],
      OBV = obv_values[length(obv_values)]
    )
    
    # Append the ad_obv_row to the ad_obv_data data frame
    ad_obv_data <- rbind(ad_obv_data, ad_obv_row)
  }
  
  marketOrders <- marketOrders
  store$entry_price <- entry_price
  store$iter <- store$iter + 1  # Increment iter
  
  # Write the ad_obv_data data frame to a CSV file
  # write.table(ad_obv_data, file = "ad_obv_data.csv", append = TRUE, row.names = FALSE, col.names = !file.exists("ad_obv_data.csv"), sep = ",")
  
  return(list(store = store, marketOrders = marketOrders,
              limitOrders1 = allzero, limitPrices1 = allzero,
              limitOrders2 = allzero, limitPrices2 = allzero))
}