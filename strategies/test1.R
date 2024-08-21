#obv_data <- data.frame()

getOrders <- function(store, newRowList, currentPos, info, params) {
  allzero <- rep(0, length(newRowList))
  
  if (is.null(store)) store <- initStoreOBV(newRowList, params$series)
  
  marketOrders <- -currentPos
  pos <- allzero  # Initialize 'pos' with zeros for all series
  
  for (i in 1:length(params$series)) {
    stock_idx <- params$series[i]
    cl <- newRowList[[stock_idx]]$Close
    volume <- newRowList[[stock_idx]]$Volume
    obv_values <- calculateOBV(cl, volume, store$prev_close[[i]], store$prev_obv[[i]])
    
    # Check if obv_values contain missing values
    if (!anyNA(obv_values)) {
      threshold1 <- params$threshold1[i]
      threshold2 <- params$threshold2[i]
      if (obv_values > threshold1) {
        if (store$entry_price[i] == 0) {
          store$entry_price[i] <- cl  # Store entry price only if it's not already set
        }
        pos[stock_idx] <- params$posSizes[i]  # Update pos for the specific series
      } else if (obv_values < threshold2) {
        if (store$entry_price[i] == 0) {
          store$entry_price[i] <- cl  # Store entry price only if it's not already set
        }
        pos[stock_idx] <- -params$posSizes[i]  # Update pos for the specific series
      }
      
      if (store$entry_price[i] != 0) {
        trade_return <- calculateTradeReturn(store$entry_price[i], cl)
        
        # Check if trade return exceeds take-profit percentage
        if (trade_return >= 0.2) {
          # Exit the position if trade return is greater than or equal to 20%
          pos[stock_idx] <- 0
          store$entry_price[i] <- 0  # Reset entry price
        }
        
        # Check if trade return falls below stop-loss threshold
        if (trade_return <= -0.1) {
          # Exit the position if trade return is less than or equal to -10%
          pos[stock_idx] <- 0
          store$entry_price[i] <- 0  # Reset entry price
        }
      }
    } else {
      pos[stock_idx] <- 0  # Set to 0 if obv_values contain missing values
    }
    # Update the OBV store for the specific stock
    store$prev_close[[i]] <- tail(cl, 1)  # Store the current close as the previous close
    store$prev_obv[[i]] <- tail(obv_values, 1)  # Store the current OBV as the previous OBV
  }
  
  marketOrders <- marketOrders + pos  # Accumulate positions for all series
  
  return(list(store = store, marketOrders = marketOrders,
              limitOrders1 = allzero, limitPrices1 = allzero,
              limitOrders2 = allzero, limitPrices2 = allzero))
}



# Initialize the store for OBV
initStoreOBV <- function(newRowList, series) {
  obv_list <- vector("list", length(series))
  prev_close_list <- vector("list", length(series))
  prev_obv_list <- vector("list", length(series))
  entry_price <- rep(0, length(series))  # Initialize entry price vector
  
  for (i in 1:length(series)) {
    obv_list[[i]] <- rep(0, length(newRowList[[series[i]]]$Close))
    prev_close_list[[i]] <- 0  # Initialize with 0
    prev_obv_list[[i]] <- 0  # Initialize with 0
  }
  
  return(list(obv = obv_list, prev_close = prev_close_list, prev_obv = prev_obv_list, entry_price = entry_price))
}


# Calculate OBV and store the result
calculateOBV <- function(close, volume, prev_close, prev_obv) {
  obv_values <- numeric(length(close))
  prev_obv <- xts(prev_obv, order.by = index(close))
  
  for (i in 1:length(close)) {
    stock_close <- close[i]
    stock_prev_close <- prev_close[[i]]
    stock_prev_obv <- prev_obv[, i]
    
    obv <- ifelse(stock_close > stock_prev_close, stock_prev_obv + volume[i],
                  ifelse(stock_close < stock_prev_close, stock_prev_obv - volume[i], stock_prev_obv))
    
    
    obv_values[i] <- ifelse(!any(is.na(obv)), tail(obv, 1), 0)  # Handle NA values gracefully
  }
  
  return(obv_values)
}

# Calculate Return and store the result
calculateTradeReturn <- function(entry_price, cl) {
  # Print the values of entry_price and cl
  # print(paste("Entry price:", entry_price))
  # print(paste("Current price:", cl))
  
  if (entry_price == 0 || cl == 0) {
    print("One or both values are zero.")
    return(0)  # Return 0 if either entry_price or current_price is not available
  }
  
  trade_return <- (as.numeric(cl) - as.numeric(entry_price)) / as.numeric(entry_price)
  # print(paste("Trade return:", trade_return))
  
  # Check if trade return exceeds take-profit percentage
  if (trade_return >= 0.05) {
    return(trade_return)
  }
  
  return(trade_return)
}




