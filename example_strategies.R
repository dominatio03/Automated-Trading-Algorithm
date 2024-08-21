example_strategies <- c("fixed", 
                        "big_spender",
                        "bankrupt", 
                        "copycat", 
                        "random", 
                        "rsi_contrarian", 
                        "bbands_trend_following",
                        "bbands_contrarian",
                        "bbands_holding_period",
                        "simple_limit",
                        "extreme_limit",
                        "test1",
                        "acc_dist",
                        "Final",
                        "team3"
                        )

example_params <- list(
                    "fixed"=list(sizes=rep(1,10)),
                    "big_spender"=list(sizes=rep(1,10)),
                    "bankrupt"=list(leverage=40000000),
                    "copycat"=NULL,
                    "random"=list(maxLots=100),
                    "rsi_contrarian"=list(lookback=10,threshold=25,series=1:5),
                    "bbands_contrarian"=list(lookback=20,sdParam=1.5,series=1:4,posSizes=rep(1,10)),
                    "bbands_trend_following"=list(lookback=50,sdParam=1.5,series=c(1,3,5,7,8,9),posSizes=rep(1,10)),
                    "bbands_holding_period"=list(lookback=50,sdParam=1.5,series=c(1,3),posSizes=rep(1,10),holdPeriod=6),
                    "simple_limit"=list(spreadPercentage=0.001,inventoryLimits=rep(10,10)),
                    "extreme_limit"=list(spreadPercentage=0.001,inventoryLimits=rep(10,10)),
                    "test1" = list(lookback=10, series = c(1,2,3,4,5,6,7,8,9.10), posSizes = rep(1,10),  #c(2,4,5,9)
                                   threshold1= c(0,0,0,0,0,0,0,0,0,0), 
                                   threshold2=c(0,0,0,0,0,0,0,0,0,0)),
                    "acc_dist" = list(lookback=10, series = c(1,2,3,4,5,6,7,8,9,10), posSizes = rep(1,10),#c(2,4,5,7,10)
                                      threshold1= c(0,0,0), 
                                      threshold2=c(0,0,0), stop_loss = 0.05), 
                    "Final" = list(series = 1:10, posSizes = rep(1,3), 
                                   spreadPercentage=0.02, takeProfitRatio =0.3,  #c(2,4,5,9)0.00912
                                   threshold1= c(0,0,0,0,0,0,0,0,0,0), 
                                   threshold2=c(0,0,0,0,0,0,0,0,0,0),
                                   risk_per_trade = c(0.0005, 0.003, 0, 0, 0, 0, 0, 0, 0.07, 0.03),
                                   budget = c(250000, 250000, 0, 400000, 200000, 0, 100000, 0, 50000, 0)
                                   ),
                    "team3" = list(adx_n = c(12,12,12,8,8,10,12,12,12,12),
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
                                                 risk_per_trade = c(0.001, 0, 0, 0, 0, 0, 0, 0, 0.07, 0),
                                                 
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
                    )

load_strategy <- function(strategy) {

    strategyFile <- file.path('strategies', paste0(strategy,'.R'))

    # load strategy
    cat("Sourcing",strategyFile,"\n")
    source(strategyFile) # load in getOrders

    # set params
    params <<- example_params[[strategy]]
    print("Parameters:")
    print(params)
}
