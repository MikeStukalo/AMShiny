### Helper functions for portfolio valuation
###
### Mikhail Stukalo, 2018 
###
library(tseries)


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


findEfficientFrontier.Return = function(zoo, target_ret, short = FALSE){

    #Calculate optimal weights
    opt.weights = portfolio.optim(returns, pm=ret/250, shorts = short)$pw
    
    if (short == FALSE){
      opt.weights = pmax(opt.weights, 0) #Correct approximation error
  
      opt.weights = opt.weights/sum(opt.weights)
    }
    
    return (opt.weights) 
}
    


