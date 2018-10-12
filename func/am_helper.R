### Helper functions for portfolio valuation
###
### Mikhail Stukalo, 2018 
###
library(tseries)
library(dplyr)
library(tibble)
library(PerformanceAnalytics)
library(DEoptim)




# Function that calculates portfolio return/risk given mean returns of assets and a covariance matrix
calcPortPerformance = function(weights, mean_ret, cov_matrix){


portRet =  t(weights) %*% mean_ret
portRisk =  sqrt(t(weights) %*% (cov_matrix %*% weights))

return (list(portRet, portRisk))
}


## Function that simulates portfolio risk/return given mean returns of assets and a covariance matrix

simPortfolios = function(mean_ret, cov_matrix, nsim=10000){
    
    n_assets = length(mean_ret) #Get number of assets
    

    #Create empty DataFrame
    result = data.frame(Return = rep(NA,nsim), Risk = rep(NA, nsim))
    
    #Simulate portfolios performance and populate the resulting dataframe
    for (i in 1:nsim){
      weights = runif(n_assets, 0, 1)  #Simulate normal distribution
      weights = weights/sum(weights)      #Make sure that weights add up to 1.0
  
      portRet = calcPortPerformance(weights, mean_ret, cov_matrix) [[1]]
      portRisk = calcPortPerformance(weights, mean_ret, cov_matrix) [[2]]
  
      result$Return[i] = portRet
      result$Risk[i] = portRisk
      }
    return (result)
}



## Function that finds weights of assets on the efficient frontier
# By target return
findEfficientFrontier.Return = function(zoo, target_ret, short = FALSE){

    #Calculate optimal weights
    opt.weights = portfolio.optim(returns, pm=target_ret/250, shorts = short)$pw
    
    if (short == FALSE){
      opt.weights = pmax(opt.weights, 0) #Correct approximation error
  
      opt.weights = opt.weights/sum(opt.weights)
    }
    
    return (opt.weights) 
}

#By target risk
findEfficientFrontier.Risk = function(mean_ret, cov_matrix, target_risk){
  
  obj_func = function(w){
    
    #To avoid NA
    if (sum(w) ==0){
      w = w + 1e-10}
    
    #Balance to one
    w = w/sum(w)
    
    #Calculate negative return
    neg_ret = - t(w) %*% mean_ret
    p_risk = sqrt(t(w) %*% cov_matrix %*% w)
    
    
    return(neg_ret + abs(p_risk - target_risk)) #Penalized optimization
  }
  
  
  # Set parameters
  controlDE <- list(reltol=.000001,steptol=150, itermax = 10000,trace = 5000,
                    strategy=1, c=0)
  
  #Long only
  N = length(mean_ret)
  lower = rep(0,N)
  upper = rep(1,N)
  
  out <- DEoptim(fn = obj_func, lower = lower, upper = upper, control = controlDE)
  
  opt_w = out$optim$bestmem
  
  opt_w = opt_w/sum(opt_w) #Sum up to 1
  
  return(opt_w)
  
}


#Function that calculates portfolio returns
calcPortReturn = function(df, from, to, wght, rebalance){
  
  #Cut dataframe to reflect date range
  df_range = df %>% rownames_to_column("date") %>%
    filter(as.Date(date)>=from & as.Date(date) <= to) %>% column_to_rownames("date")
  
  df_range = xts(df_range, order.by = as.Date(row.names(df_range)))
  indexClass(df_range) <- "Date"
  
  #Create repalace operator
  reb_op = ifelse(rebalance=="Never", NA,
                  ifelse(rebalance=="Annually", "years", 
                  ifelse(rebalance=="Quarterly", "quarters",
                         "months")))
  
  port_ret = Return.portfolio(df_range, weights = wght, geometric = T, rebalance_on = reb_op)
  
  port_ret = data.frame(port_ret)
  
  colnames(port_ret) = c("RetPort")
  
  return (port_ret)
  
}


#Function that calculates portfolio performance measures

calcPortMeasures = function (port_ret, benchmark, rf){
  
  mean_rf = mean(rf)
  mean_port_ret = mean(port_ret)
  sd_port_ret = sd(port_ret)
  
  #Calculate Sharpe
  sharpe = ((mean_port_ret - mean_rf) / sd_port_ret) * sqrt(250)
  
  #Calculate Beta
  mod = lm(formula = port_ret~benchmark)
  beta = summary(mod)$coefficients[2,1]
  
  #Calculate Sortino
  sortino = SortinoRatio(port_ret) * sqrt(250)
  
  #Calculate Taylor
  treynor = ((mean_port_ret - mean_rf)*250)*100/beta
  
  results = list("AvRet"=mean_port_ret * 250, "StDev" = sd_port_ret * sqrt(250),
                 "Sharpe" = sharpe, "Sortino" = sortino[1], "Beta" = beta, "Treynor" = treynor)
  
  return (results)
  
}


