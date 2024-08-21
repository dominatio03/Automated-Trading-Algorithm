library(TTR)
library(quantmod)


maxRows <- 1500

params <- list(adx_n = c(12,12,12,8,8,10,12,12,12,12),
               macd_fast_period =  c(9, 9, 9, 9, 9, 9, 9, 9, 9, 9),
               macd_slow_period = c(21.5,21.5,21.5,21.5,26,21.5,21.5,21.5,21.5,21.5),     
               macd_signal_period = c(7,7,7,9,9,7,7,7,7,7),     
               strong_trend_level = c(38,38,38,25,28,38,38,38,38),
               series = 1:10,
               spreadPercentage=0.001,
               rsi_n = c(14,14,14,14,14,14,14,14,14,14),
               rsi_overbought = c(80,80,80,80,80,80,80,80,80,80),
               rsi_oversold = c(16,16,16,16,16,16,16,16,16,16),
               ####################################################
               
               #OBV PARAMS
               threshold1= c(0,0,0,0,0,0,0,0,0,0), 
               threshold2=c(0,0,0,0,0,0,0,0,0,0),
               risk_per_trade = c(0.0005, 0.01, 0, 0, 0, 0, 0, 0, 0.07, 0),
               
               #POSIION SIZING PARAMS
               ########################################################
               atrmin = c(3.0292580,0,0,5.414275,5.300149, 1.511366,0,7.385845,2.6403753,0),
               atrmax = c(14.1850892,0,0,17.54393,14.95692,  2.544651 ,0, 15.528080,6.6918340,0),
               #########################################################
               
               
               #Z-SCORE PARAMS
               smas = c(0,0,0,0,0,32,49,0,0,0),  
               entryThreshold = c(0,0,0,0,0,2.1,2,0,0,0),  
               StopLossCondition = -0.5,
               TakeProfitCondition = c(0,0,0,0,0,0.175,0.025,0,0,0),
               ExitThreshold = c(0,0,0,0,0,0,0,0,0,0))


initClStore  <- function(newRowList,series) {
  clStore <- matrix(0,nrow=maxRows,ncol=length(series))
  return(clStore)
}
initHiStore  <- function(newRowList,series) {
  HiStore <- matrix(0,nrow=maxRows,ncol=length(series))
  return(HiStore)
}
initLoStore  <- function(newRowList,series) {
  LoStore <- matrix(0,nrow=maxRows,ncol=length(series))
  return(LoStore)
}
initVoStore  <- function(newRowList,series) {
  VoStore <- matrix(0,nrow=maxRows,ncol=length(series))
  return(VoStore)
}
initOpStore <- function(newRowList,series) {
  OpStore <- matrix(0,nrow=maxRows,ncol=length(series))
  return(OpStore)
}

initStore <- function(newRowList, series) {
  store <- list(
    iter = 0,
    data = newRowList,
    cl = initClStore(newRowList, series),
    h = initHiStore(newRowList, series),
    l = initLoStore(newRowList, series),
    v = initVoStore(newRowList, series),
    op = initOpStore(newRowList, series),
    prevMacd = rep(NA, length(series)),
    prevSignal = rep(NA, length(series)),
    prevAdx = rep(NA, length(series)),
    entry = rep(NA, length(series)),
    trades = list(),
    garchPositionSizes = list(), # New field for storing position sizes
    obv_list = vector("list", length(series)),
    prev_close_list = vector("list", length(series)),
    prev_obv_list = vector("list", length(series)),
    prev_ad_list = vector("list", length(series)),
    entry_price = vector("numeric", length(series)),
    budget = c(150000, 0, 0, 0.4, 0.2, 0.1, 0.1, 0, 50000, 0),
    entryPrice = rep(NA, length(series)),
    peakPrice = rep(NA, length(series)),
    troughPrice = rep(NA, length(series)),
    atr = rep(NA, length(series)),
    prevZscore = rep(NA, length(series)),
    Trades = lapply(1:length(newRowList), function(x) list(direction = NULL, entryPrice = NULL)),
    
    ActiveTrades = data.frame()
    
  )
  
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
  for (i in 1:length(series))
    HiStore[iter,i] <- as.numeric(newRowList[[series[i]]]$High)
  return(HiStore)
}
updateLoStore <- function(LoStore, newRowList, series, iter) {
  for (i in 1:length(series))
    LoStore[iter,i] <- as.numeric(newRowList[[series[i]]]$Low)
  return(LoStore)
}
updateVoStore <- function(VoStore, newRowList, series, iter) {
  for (i in 1:length(series))
    VoStore[iter,i] <- as.numeric(newRowList[[series[i]]]$Volume)
  return(VoStore)
}
updateClStore <- function(clStore, newRowList, series, iter) {
  for (i in 1:length(series))
    clStore[iter,i] <- as.numeric(newRowList[[series[i]]]$Close)
  return(clStore)
}
updateOpStore <- function(opStore, newRowList, series, iter) {
  for (i in 1:length(series))
    opStore[iter,i] <- as.numeric(newRowList[[series[i]]]$Open)
  return(opStore)
}
updateStore <- function(store, newRowList, series) {
  store$iter <- store$iter + 1
  store$h <- updateHiStore(store$h,newRowList,series,store$iter)
  store$l <- updateLoStore(store$l,newRowList,series,store$iter)
  store$cl <- updateClStore(store$cl,newRowList,series,store$iter)
  store$v <- updateVoStore(store$v,newRowList,series,store$iter)
  store$op <- updateOpStore(store$op,newRowList,series,store$iter)
  store$entry <- store$entry
  return(store)
}

calculateOBV <- function(close, volume, prev_close, prev_obv) {
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

getOrders <- function(store, newRowList, currentPos, info, params) {
  allzero <- rep(0, length(newRowList))
  limitOrders1 = allzero
  limitOrders2 = allzero
  marketOrders = allzero
  
  if (is.null(store)) {
    store <- initStore(newRowList, params$series)
  }
  
  store <- updateStore(store, newRowList, params$series)
  
  # Strategy for MACD/ADX on series 4 and 5
  tradingseries <- c(1,2,4,5,6,7,9,8)
  
  # Strategy for OBV/AD on series 1, 2, 9, 10
  
  
  minDataPointsRequired <- 50
  
  
  for (i in params$series) {
    if(!i %in% tradingseries){
      next
    }
    
    if (i == 4 || i== 5) {
      if (store$iter >= minDataPointsRequired) {
        macdCalc <- MACD(store$cl[1:store$iter,i], 
                         nFast = params$macd_fast_period[i], 
                         nSlow = params$macd_slow_period[i], 
                         nSig = params$macd_signal_period[i], 
                         percent = TRUE)
        
        macd <- tail(macdCalc[,1], 1)
        signal <- tail(macdCalc[,2], 1)
        
        HLC <- cbind(
          High = store$h[1:store$iter, i],
          Low = store$l[1:store$iter, i],
          Close = store$cl[1:store$iter, i])
        #print(HLC)
        
        adxCalc <- ADX(HLC, n = params$adx_n[i])
        adx <- tail(adxCalc[, 1], 1) 
        
        
        currentATR <- calculateCurrentATR(store, i) 
        
        
        ma50 <- rollmean(store$cl[1:store$iter, i], 50, fill = NA, align = 'right')
        ma200 <- rollmean(store$cl[1:store$iter, i], 200, fill = NA, align = 'right')
        
        
        currentMa50 <- tail(ma50, 1)
        currentMa200 <- tail(ma200, 1)
        
        currentPrice <- store$cl[store$iter, i]
        
        
        
        tradeID <- paste("Trade", i, "Entry", store$iter, sep="_") 
        
        # Entering Long Position
        if (!is.na(macd) && !is.na(signal) && !is.na(adx) &&
            !is.na(currentMa50) && !is.na(currentMa200) && !is.na(currentPrice) &&
            macd > signal && macd > 0 && signal > 0 && adx > params$strong_trend_level[i] &&
            currentMa50 > currentMa200 && currentPrice > currentMa200) {
          
          store$trades[[tradeID]] <- list(type = "Long", entryIndex = store$iter, status = "open")
          #record price traded at 
          
          positionSize <- calculatePositionSize(atr = currentATR, atrMin = params$atrmin[i], atrMax = params$atrmax[i], sizeMin = 12, sizeMax = 20)
          price <- store$cl[store$iter, i] * positionSize
          
          #(store$budget[i])
          
          
          if ((price < (store$budget[i] * info$balance) && info$balance > 100000)) {
            limitOrders1[i] <-  1 * positionSize
            # store$budget[i] <- store$budget[i] - price
          }
          
        }
        
        
        
        # Entering Short Position
        if (!is.na(macd) && !is.na(signal) && !is.na(adx) &&
            !is.na(currentMa50) && !is.na(currentMa200) && !is.na(currentPrice) &&
            macd < signal && macd < 0 && signal < 0 && adx > params$strong_trend_level[i] &&
            currentMa50 < currentMa200 && currentPrice < currentMa200) {
          
          store$trades[[tradeID]] <- list(type = "Short", entryIndex = store$iter, status = "open")
          positionSize <- calculatePositionSize(atr = currentATR, atrMin = params$atrmin[i], atrMax = params$atrmax[i], sizeMin = 5, sizeMax = 20)
          price <- store$cl[store$iter,i] * positionSize
          
          
          if ((price < (store$budget[i] * info$balance) && info$balance > 100000)) {
            limitOrders2[i] <-  -1 * positionSize
            # store$budget[i] <- store$budget[i] - price
          }
          
        }
        
        
        
        if (!is.na(currentPos[i]) && currentPos[i] != 0) {
          rsi <- RSI(store$cl[, i], n = params$rsi_n[i])
          
          # Assuming you sell/short when overbought and buy when oversold
          if (currentPos[i] > 0 && rsi[store$iter] > params$rsi_overbought[i]) {
            # If currently long and the asset is overbought, exit position (go neutral or short)
            limitOrders2[i] <- -currentPos[params$series[i]]
          } else if (currentPos[i] < 0 && rsi[store$iter] < params$rsi_oversold[i]) {
            # If currently short and the asset is oversold, exit position (go neutral or long)
            limitOrders1[i] <- -currentPos[params$series[i]]
          }
        }
      }
      
    } else if (i == 7 || i == 6) {
      # print("ksdnfklsndf")
      smaDays <- params$smas[i]
      allZScores = c()
      
      if (store$iter >= 20){ 
        ## sma <- (mean(tail(store$data[[i]]$Close[],smaDays)))
        ## sdev <- (sd(tail(store$data[[i]]$Close[],smaDays)))
        ## closeValue <- as.numeric(tail(store$data[[i]]$Close[],1))
        sma <- mean(tail(store$cl[1:store$iter, i],smaDays))
        sdev <- sd(tail(store$cl[1:store$iter, i],smaDays))
        closeValue <- store$cl[store$iter,i]
        currentATR <- calculateCurrentATR(store, i) 
        positionSize <- calculatePositionSize(atr = currentATR, 
                                              atrMin = params$atrmin[i], 
                                              atrMax = params$atrmax[i], 
                                              sizeMin = 20, 
                                              sizeMax = 30)
        
        #print(positionSize)
        if (sdev != 0){
          zScore <- (closeValue - sma) / sdev
          #print(zScore)
          exitCondition = params$ExitThreshold[i]
          entryThreshold = params$entryThreshold[i]
          
          if (currentPos[i] == 0){ # No Current Position
            if(zScore >= entryThreshold){
              price <- store$cl[store$iter, i] * positionSize
              #print(store$cl[store$iter, i])
              #print(price)
              #print(store$budget[i])
              
              if ((price < (store$budget[i] * info$balance) && info$balance > 100000)) {
                
                # Entry condition modified
                marketOrders[i] <- -1 * positionSize
                #store$budget[i] <- store$budget[i] - price
                #print("market order made")
              }
              
            }else if (zScore <= -entryThreshold){ 
              price <- store$cl[store$iter,i] * positionSize
              
              #print(store$budget[i])
              
              if ((price < (store$budget[i] * info$balance) && info$balance > 100000)) {
                
                
                marketOrders[i] <- 1 * positionSize
                #store$budget[i] <- store$budget[i] - price
                #print("market order made")
              }
              
            }else{
              marketOrders[i]  <- 0 
            }
          }else if (currentPos[i] == 1){ #Current Long Position (z <= -2)
            if(zScore >= -exitCondition){
              price <- store$cl[store$iter,i] * positionSize
              
              #print(store$budget[i])
              
              marketOrders[i] <- -1 * positionSize
              store$budget[i] <- store$budget[i] - price
              #print("market order made")
              
            }else{
              marketOrders[i]  <- 0
            }
          }else if (currentPos[i] == -1){ #Current Short Position (z >= 2)
            if(zScore <= exitCondition){
              price <- store$cl[store$iter, i] * positionSize
              
              #print(store$budget[i])
              
              marketOrders[i] <- 1 * positionSize
              #store$budget[i] <- store$budget[i] - price
              #print("market order made")
              
              
            }else{
              marketOrders[i]  <- 0
            }
          }
          
        }else{
          marketOrders <- allzero #COULDN'T CALCUALTE SMA CUZ = 0
          
        }
      }
      
      if (any(marketOrders != 0)) {
        trades <- which(marketOrders != 0)
        #print(trades)
        
        for (s in trades){
          # Define a new row of data
          new_row <- data.frame(
            Iteration = store$iter,          
            Series = s,             
            PositionSize = marketOrders[s],     
            EntryPrice = NA,
            CurrentPrice = store$cl[store$iter,s], #as.numeric(tail(store$cl[[s]]$Close[],1)),
            PercentageChange = NA
          )
          store$ActiveTrades <- rbind(store$ActiveTrades, new_row)
          #print(store$ActiveTrades)
          #store$data[[s]]$Open[(store$iter - 1)]
        }
      } else {
      }
      
      ## checks prices for entry condition
      row_index <- 1
      while (row_index <= nrow(store$ActiveTrades)) {
        if (store$iter > store$ActiveTrades$Iteration[row_index]) {
          s = store$ActiveTrades$Series[row_index]
          iteration = store$ActiveTrades$Iteration[row_index]
          entryPrice = as.numeric(store$op[iteration,s]) #store$data[[s]]$Open[iteration]
          currentPrice = store$cl[store$iter,s] #as.numeric(tail(store$data[[s]]$Close, 1))
          
          
          
          percentageDif = 0
          percentageDifLong = round(((currentPrice - entryPrice) / entryPrice), 2)
          percentageDifShort = round(((entryPrice - currentPrice) / entryPrice), 2)
          
          #print(entryPrice)
          #print(currentPrice)
          
          #positive number = profit
          #negative number = exit
          
          
          positionSize = store$ActiveTrades$PositionSize[row_index]
          store$ActiveTrades$EntryPrice[row_index][is.na(store$ActiveTrades$EntryPrice[row_index])] <- entryPrice
          store$ActiveTrades$CurrentPrice[row_index] <- currentPrice
          
          
          stopLossCondition = params$StopLossCondition
          takeProfitCondition = params$TakeProfitCondition[i]
          
          
          if (store$ActiveTrades$PositionSize[row_index] < 0){
            #short trade
            store$ActiveTrades$PercentageChange[row_index] <- percentageDifShort
          }else if (store$ActiveTrades$PositionSize[row_index] > 0){
            #long trade 
            store$ActiveTrades$PercentageChange[row_index] <- percentageDifLong
          }
          
          if (store$ActiveTrades$PercentageChange[row_index] <= stopLossCondition || store$ActiveTrades$PercentageChange[row_index] >= takeProfitCondition) {
            #(paste(" STOPLOSS - series", s, "-  % loss =", store$ActiveTrades$PercentageChange[row_index]))
            counterPosition = -1 * positionSize
            marketOrders[s] <- counterPosition
            #print(marketOrders)
            #print("market order made")
            
            
            # Remove position from DF now exited
            store$ActiveTrades <- store$ActiveTrades[-row_index, ]
            
            #print(paste("removed row", row_index))
            # Do not increment row_index because we removed the current row,
            # so next row takes its place
          } else {
            # Only increment row_index if no row was removed
            row_index <- row_index + 1
          }
        } else {
          # Increment row_index if the condition is not met
          row_index <- row_index + 1
        }
      }
      
      
      
      
    } else if (i == 1 || i == 9) {
      
      trading_allowed <-  vector("list", length(params$series))
      marketOrders[i] <- -currentPos[i]
      allzero <- rep(0, length(newRowList))
      pos <- allzero
      
      entry_price <- store$entry_price
      cumulative_return <- vector("numeric", length(params$series))
      lookback <- params$lookback_medium
      stopRate <- 0.3
      takeProfitRate <- 0.3
      new_orders <- allzero
      
      for (i in c(1,9)) {
        stock_idx <- params$series[i]
        cl <- newRowList[[stock_idx]]$Close
        volume <- newRowList[[stock_idx]]$Volume
        high <- newRowList[[stock_idx]]$High
        low <- newRowList[[stock_idx]]$Low
        
        risk_per_trade <- ifelse(i == 1, params$risk_per_trade[1],
                                 ifelse(i == 9, params$risk_per_trade[9]))
        
        position_size <- (risk_per_trade * store$budget[i]) / (stopRate * cl)
        price <- cl * position_size
        
        obv_values <- calculateOBV(cl, volume, store$prev_close[[i]], store$prev_obv[[i]])
        ad_values <- calculateAD(cl, high, low, volume, store$prev_ad[[i]])
        
        if (!any(is.na(obv_values)) && !any(is.na(ad_values))) {
          
          threshold1 <- params$threshold1[i]
          threshold2 <- params$threshold2[i]
          
          if (obv_values > threshold1 && ad_values > threshold1) {
            if (price < store$budget[i] && store$budget[i] > 1000 && info$balance > 120000) {
              if (i == 9) { 
                marketOrders[i] <- -position_size
                store$budget[i] <- (info$balance * 0.05) - price
              } else {  
                marketOrders[i] <- position_size
                store$budget[i] <- (info$balance * 0.25) - price
              }
              entry_price[i] <- cl
              cumulative_return[i] <- 0
            }
          } else if (obv_values < threshold2 && ad_values < threshold2) {
            if (price < store$budget[i] && store$budget[i] > 1000 && info$balance > 120000) {
              if (i == 9) { 
                marketOrders[i] <- position_size
                store$budget[i] <- (info$balance * 0.05) - price
              } else {  
                marketOrders[i] <- -position_size
                store$budget[i] <- (info$balance * 0.25) - price
              }
              entry_price[i] <- cl
              cumulative_return[i] <- 0
            }
          }
        }
        
        daily_return <- (cl - entry_price[stock_idx]) / entry_price[stock_idx]
        cumulative_return[stock_idx] <- cumulative_return[stock_idx] + daily_return * currentPos[stock_idx]
        
        if (!is.na(cumulative_return[stock_idx])) {
          if (cumulative_return[stock_idx] >= takeProfitRate) {
            pos[stock_idx] <- 0
            trading_allowed[stock_idx] <- FALSE  # Disable trading for this series
          } else if (cumulative_return[stock_idx] <= -stopRate) {
            pos[stock_idx] <- 0
            trading_allowed[stock_idx] <- FALSE  # Disable trading for this series
          }
        }
        
        store$prev_close[[i]] <- tail(cl, 1)
        store$prev_obv[[i]] <- tail(obv_values, 1)
        store$prev_ad[[i]] <- tail(ad_values, 1)
      }
      
      store$entry_price <- entry_price
    }
  }
  
  
  
  # Calculate spread
  spread <- sapply(newRowList, function(x) params$spreadPercentage * (x$High - x$Low))
  
  limitPrices1 <- sapply(1:length(newRowList), function(i) newRowList[[i]]$Close - spread[i] / 2)
  limitPrices2 <- sapply(1:length(newRowList), function(i) newRowList[[i]]$Close + spread[i] / 2)
  
  
  return(list(store=store,marketOrders=marketOrders,
              limitOrders1=limitOrders1,limitPrices1=limitPrices1,
              limitOrders2=limitOrders2,limitPrices2=limitPrices2))
}





calculatePositionSize <- function(atr, atrMin, atrMax, sizeMin, sizeMax) {
  #print("entering function calulate position size ")
  atrNormalized <- (atr - atrMin) / (atrMax - atrMin)
  
  atrNormalized <- max(min(atrNormalized, 1), 0)
  positionSize <- round(sizeMin + (sizeMax - sizeMin) * (1 - atrNormalized))
  #print(positionSize)
  return(positionSize)
}


library(TTR)

# Function to calculate current ATR
calculateCurrentATR <- function(store, i) {
  
  
  HLC <- cbind(
    High = store$h[1:store$iter, i],
    Low = store$l[1:store$iter, i],
    Close = store$cl[1:store$iter, i])
  
  atrCalc <- ATR(HLC, n = 14)
  currentATR <- tail(atrCalc[, 1], 1)
  
  return(currentATR)
}
