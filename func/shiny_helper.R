# Miscellaneous functions
updateweight = function(oldweight, new, i) {
  if (new==oldweight[i]) {
    oldweight
  } else {
    newweight = rep(0,6)
    oldweight = oldweight 
    newweight[-i] = oldweight[-i]/(sum(oldweight[-i]) + 1e-10)*(1-new)
    newweight[i] = new
    newweight
  }
}

# suspend and resume a list of observers
suspendMany = function(observers) invisible(lapply(observers, function(x) x$suspend()))
resumeMany = function(observers) invisible(lapply(observers, function(x) x$resume()))

# function to change sliderInput
wghtsliderInput = function(inputId,value, label, submitted=FALSE) {
  if (!submitted)
    sliderInput(inputId=inputId,
                value=value,
                label=label,
                min=0,
                max=1,
                ticks=FALSE)
}



### Function to perform backtesting
bt_port = function(df, from, to, wght, rebalance){
  
  # Create a dataframe with portfolio and benchmark returns
  
  
  df_tmp = df %>% mutate(date = as.Date(row.names(df)))
  
  
  # Portfolio return
  port_ret = data.frame(calcPortReturn(df, from, to, wght, rebalance))
  
  port_ret$date = as.Date(row.names(port_ret))
  
  port_ret = rename(port_ret, Portfolio = RetPort)
  
  # 60/30/10 Portfolio
  sixty_port = data.frame(calcPortReturn(df, from, to,
                                         wght = c(0.6, 0, 0, 0.1, 0.3, 0), rebalance))
  
  sixty_port$date = as.Date(row.names(sixty_port))
  sixty_port = rename(sixty_port, R60T10C30 = RetPort)
  
  # Merge into one df
  port_ret = merge(port_ret, df_tmp[,c("Russell2000","date")], by = "date", all.x = TRUE)
  port_ret = merge(port_ret, sixty_port, by = "date", all.x = TRUE)
  
  
  return(port_ret)
}
