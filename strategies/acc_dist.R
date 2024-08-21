getOrders <- function(store, newRowList, currentPos, info, params) {
  allzero <- rep(0, length(newRowList))
  
  if (is.null(store)) store <- initStoreAD(newRowList, params$series)
  
  marketOrders <- -currentPos
  pos <- allzero  # Initialize 'pos' with zeros for all series
  
  entry_price <- vector("numeric", length(params$series))
  
  for (i in 1:length(params$series)) {
    stock_idx <- params$series[i]
    cl <- newRowList[[stock_idx]]$Close
    high <- newRowList[[stock_idx]]$High
    low <- newRowList[[stock_idx]]$Low
    volume <- newRowList[[stock_idx]]$Volume
    
    ad_values <- calculateAD(cl, high, low, volume, store$prev_ad[[i]])
    
    # Check A/D and generate trading signals for each stock
    if (!any(is.na(ad_values))) {
      threshold1 <- params$threshold1[i]
      threshold2 <- params$threshold2[i]
      if (ad_values > threshold1) {
        pos[stock_idx] <- params$posSizes[i]  # Update pos for the specific series
        entry_price[i] <- cl
      } else if(ad_values < threshold2) {
        pos[stock_idx] <- -params$posSizes[i]  # Update pos for the specific series
        entry_price[i] <- cl
      }
    } else {
      pos[stock_idx] <- 0  # Set to 0 if ad_values are empty
    }
    
    # Update the A/D store for the specific stock
    store$prev_ad[[i]] <- tail(ad_values, 1)  # Store the current A/D as the previous A/D
  }
  
  marketOrders <- marketOrders + pos  # Accumulate positions for all series
  
  return(list(store = store, marketOrders = marketOrders,
              limitOrders1 = allzero, limitPrices1 = allzero,
              limitOrders2 = allzero, limitPrices2 = allzero))
}


# Initialize the store for A/D
initStoreAD <- function(newRowList, series) {
  ad_list <- vector("list", length(series))
  prev_ad_list <- vector("list", length(series))
  
  for (i in 1:length(series)) {
    ad_list[[i]] <- rep(0, length(newRowList[[series[i]]]$Close))
    prev_ad_list[[i]] <- 0  # Initialize with 0
  }
  
  return(list(ad = ad_list, prev_ad = prev_ad_list))
}

# Calculate A/D and store the result
calculateAD <- function(close, high, low, volume, prev_ad) {
  ad_values <- numeric(length(close))
  prev_ad <- xts(prev_ad, order.by = index(close))
  
  for (i in 1:length(close)) {
    ad <- ((close[i] - low[i]) - (high[i] - close[i])) / (high[i] - low[i]) * volume[i] + prev_ad[i]
    ad_values[i] <- tail(ad, 1)
  }
  return(ad_values)
}