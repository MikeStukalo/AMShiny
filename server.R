
source('./func/am_helper.R')
source('./func/shiny_helper.R')

my_colors = brewer.pal(6, "Blues")

shinyServer(function(input, output, session){
  
  ###############
  ##  Static html pages
  ###############
  
  output$disclaimer = renderUI(includeHTML("./html/disclaimer.html"))
  output$abt = renderUI(includeHTML("./html/about.html"))
  output$measures= renderUI(withMathJax(includeHTML("./html/measures.html")))
  
  
  
  
  
  
  ###############
  ##  Theory Page
  ###############
  
  # Render graphs for Theory part (ggplot comes from global.R)
  output$graph1 =renderPlotly(g1)
  output$graph2 =renderPlotly(g2)
  output$graph3 =renderPlotly(g3)
  output$graph4 =renderPlotly(g4)
  
  
  
  ###############
  ##  Allocation Page
  ###############
  
  #Weights (make sure that sliders are mutually dependent and weights add up to 1)
  # Initialize portfolio weights
  port_weight = reactiveValues(weight=rep(1/6, 6)) # naive diversification
  
  # If any of the sliders change, then recalculate other weight weights to satisfy sum to 1 constraint
  observers = list(
    observeEvent(input$p1,
                 {
                   suspendMany(observers) #This function comes from shinyhelper.R
                   port_weight$weight = updateweight(port_weight$weight, input$p1, 1)
                   resumeMany(observers) #This function comes from shinyhelper.R
                 }
    ),
    observeEvent(input$p2,
                 {
                   suspendMany(observers)
                   port_weight$weight = updateweight(port_weight$weight, input$p2, 2)
                   resumeMany(observers)
                 }
    ),
    observeEvent(input$p3,
                 {
                   suspendMany(observers)
                   port_weight$weight = updateweight(port_weight$weight, input$p3, 3)
                   resumeMany(observers)
                 }
    ),
    observeEvent(input$p4,
                 {
                   suspendMany(observers)
                   port_weight$weight = updateweight(port_weight$weight, input$p4, 4)
                   resumeMany(observers)
                 }
    ),
    observeEvent(input$p5,
                 {
                   suspendMany(observers)
                   port_weight$weight = updateweight(port_weight$weight, input$p5, 5)
                   resumeMany(observers)
                 }
    ),
    observeEvent(input$p6,
                 {
                   suspendMany(observers)
                   port_weight$weight = updateweight(port_weight$weight, input$p6, 6)
                   resumeMany(observers)
                 }
    )
  )
  
  # If the weights change, update the sliders
  output$p1ui = renderUI({
    wghtsliderInput("p1", port_weight$weight[1], label = "Russell 2000") #This function comes from shinyhelper.R
  })
  output$p2ui = renderUI({
    wghtsliderInput("p2", port_weight$weight[2], label = "Europe Stocks")
  })
  output$p3ui = renderUI({
    wghtsliderInput("p3", port_weight$weight[3], label = "Emerging Market Stocks")
  })
  output$p4ui = renderUI({
    wghtsliderInput("p4", port_weight$weight[4], label = "US. Treasury")
  })
  output$p5ui = renderUI({
    wghtsliderInput("p5", port_weight$weight[5], label = "US. Corporate Bonds")
  })
  output$p6ui = renderUI({
    wghtsliderInput("p6", port_weight$weight[6], label = "Real Estate")
  })
  
  
  #Date slider
  #If min date and max date are the same - reset the slider
  observeEvent(input$date_range,{
    if(input$date_range[1] == input$date_range[2]){
      updateSliderTextInput(session,"date_range",selected = c(date_choices[1],date_choices[length(date_choices)]))
    }
  })
  
  
  #Allocation pie chart
  output$graph5 = renderPlotly({
  
  alloc = data.frame(wght = port_weight$weight, asset = c("Russell2000","EuropeStocks","EMStocks","Treasury","CorpBond","RealEstate"))
  
  
  
  g5 = plot_ly(alloc, labels = ~asset, values = ~wght, type = 'pie',
               textposition = 'inside',
               textinfo = 'label+percent',
               insidetextfont = list(color = '#000'),
               hoverinfo = 'text',
               text = ~paste(round(wght,4)*100, ' %'),
               marker = list(colors = my_colors,
                             line = list(color = '#FFFFFF', width = 1)),
               showlegend = FALSE, width=250, height=250) %>%
    layout(xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
           yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
           paper_bgcolor='rgba(0,0,0,0)',
           plot_bgcolor='rgba(0,0,0,0)',
           margin = list(b = 0, l = 0, t = 0))
  
  g5
  
  })
  
  #############################################
  # Perform backtesting  
  # Functions are in shiny_helper.R
  #############################################
  
  # Backtest data
  bt_data = reactive({bt_port(df, as.Date(input$date_range[1]), as.Date(input$date_range[2]), port_weight$weight, input$rebalance)})

  # Optimal portfolio data
  opt_weights = reactive({
    #Calculate target risk and return
    bt_df = bt_data()
    target_ret = mean(bt_df$Portfolio) * 250
    target_risk = sd(bt_df$Portfolio) * sqrt(250)
    
    #Extract dataframe for dates 
    from = as.Date(input$date_range[1])
    to = as.Date(input$date_range[2])
    
    df_tmp = df %>% rownames_to_column("date") %>%
      filter(as.Date(date)>=from & as.Date(date) <= to) %>% column_to_rownames("date")
    
    # Calculate inputs for optimization
    returns = xts(df_tmp, order.by = as.Date(row.names(df_tmp)))
    mean_ret = apply(df_tmp, 2, mean) * 250
    cov_matrix = cov(df_tmp) * 250
    
    #Find optimal weights
    #opt_w_ret = findEfficientFrontier.Return(returns, target_ret)
    opt_w_ret = findEfficientFrontier.ReturnALT(mean_ret, cov_matrix, target_ret)
    
    opt_w_risk = findEfficientFrontier.Risk(mean_ret, cov_matrix, target_risk)
    
    #Return a dataframe
    opt_df = data.frame(OptRet = opt_w_ret, OptRisk = opt_w_risk)
    
    return (opt_df)
    
      
  })
  
  
  #Plot backtest compound return
  output$graph6 = renderPlotly({

    input$go
    
    isolate({  ### To let weights settle
    
    bt_df = bt_data()

    #Calculate compound return
    bt_df = bt_df %>%
      gather(key="Asset", value="Return", -date) %>%
      group_by(Asset) %>%
      arrange(date) %>%
      mutate(cumRet = cumprod(1+Return) - 1) %>%
      select(date, Asset, cumRet) %>%
      spread(key=Asset, value=cumRet)

    #Plot
    plot_ly(bt_df, x = ~date, y = ~Portfolio, type = "scatter", mode = "line", name = "Portfolio",
            line = list(color = "Steelblue3", width = 2), width = 700, height = 400) %>%
      add_trace(y= ~Russell2000, name = "Russell2000",
                line = list(color = "black", width = 2)) %>%
      add_trace(y= ~R60T10C30, name = "Russel:60%, CorpBonds:30%, Treasury:10%",
                line = list(color = "gray", width = 2)) %>%
      layout(xaxis = list(title = "", showgrid = FALSE, zeroline = TRUE, showticklabels = TRUE),
             yaxis = list(title = "", showgrid = TRUE, zeroline = TRUE, showticklabels = TRUE),

             legend = list(orientation = "h", x = 0.1, y=1.2),
             paper_bgcolor='rgba(0,0,0,0)',
             plot_bgcolor='rgba(0,0,0,0)',
             margin = list(b = 20, l = 20, t = 30))
    })
  })

  #Create backtest preformance stats

  output$bt_table1 = renderTable(digits =4, {

    input$go
    
    isolate({
    #Select data
    ret_df = bt_data()

    ret_df = ret_df %>% rename(Russell=Russell2000, Mixed = R60T10C30) %>%
      select(date, Portfolio, Russell, Mixed)

    rf_range = rf%>% filter(as.Date(date) >= as.Date(input$date_range[1]) & as.Date(date) <= as.Date(input$date_range[2]))


    #Calculate performance measures
    perf_df = data.frame(Measure = c("Return (annualized)","Risk (annualized)","Sharpe","Sortino","Beta","Treynor"))
    perf_df$Portfolio = unlist(calcPortMeasures(ret_df$Portfolio, ret_df$Russell, rf_range$rf))
    perf_df$Russell = unlist(calcPortMeasures(ret_df$Russell, ret_df$Russell, rf_range$rf))
    perf_df$Mixed = unlist(calcPortMeasures(ret_df$Mixed, ret_df$Russell, rf_range$rf))

    return (perf_df)
    })
  })
  

  
  ###########
  ##  Plots for comparison
  ############
  
  #Current allocation
  output$graph7 = renderPlotly({
    
    alloc = data.frame(wght = port_weight$weight, asset = c("Russell2000","EuropeStocks","EMStocks","Treasury","CorpBond","RealEstate"))
    
    
    
    g7 = plot_ly(alloc, labels = ~asset, values = ~wght, type = 'pie',
                 textposition = 'inside',
                 textinfo = 'label+percent',
                 insidetextfont = list(color = '#000'),
                 hoverinfo = 'text',
                 text = ~paste(round(wght,4)*100, ' %'),
                 marker = list(colors = my_colors,
                               line = list(color = '#FFFFFF', width = 1)),
                 showlegend = FALSE, width=250, height=250) %>%
      layout(xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             paper_bgcolor='rgba(0,0,0,0)',
             plot_bgcolor='rgba(0,0,0,0)',
             margin = list(b = 0, l = 0, t = 0))
    
    g7
    
  })
  
  #Same return
  output$graph8 = renderPlotly({
    
    opt_w = opt_weights()
    
    alloc = data.frame(wght = opt_w$OptRet, asset = c("Russell2000","EuropeStocks","EMStocks","Treasury","CorpBond","RealEstate"))
    
    
    
    g8 = plot_ly(alloc, labels = ~asset, values = ~wght, type = 'pie',
                 textposition = 'inside',
                 textinfo = 'label+percent',
                 insidetextfont = list(color = '#000'),
                 hoverinfo = 'text',
                 text = ~paste(round(wght,4)*100, ' %'),
                 marker = list(colors = my_colors,
                               line = list(color = '#FFFFFF', width = 1)),
                 showlegend = FALSE, width=250, height=250) %>%
      layout(xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             paper_bgcolor='rgba(0,0,0,0)',
             plot_bgcolor='rgba(0,0,0,0)',
             margin = list(b = 0, l = 0, t = 0))
    
    g8
    
  })
  
  #Same Risk
  output$graph9 = renderPlotly({
    
    opt_w = opt_weights()
    
    alloc = data.frame(wght = opt_w$OptRisk, asset = c("Russell2000","EuropeStocks","EMStocks","Treasury","CorpBond","RealEstate"))
    
    
    
    g9 = plot_ly(alloc, labels = ~asset, values = ~wght, type = 'pie',
                 textposition = 'inside',
                 textinfo = 'label+percent',
                 insidetextfont = list(color = '#000'),
                 hoverinfo = 'text',
                 text = ~paste(round(wght,4)*100, ' %'),
                 marker = list(colors = my_colors,
                               line = list(color = '#FFFFFF', width = 1)),
                 showlegend = FALSE, width=250, height=250) %>%
      layout(xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             paper_bgcolor='rgba(0,0,0,0)',
             plot_bgcolor='rgba(0,0,0,0)',
             margin = list(b = 0, l = 0, t = 0))
    
    g9
    
  })
  
  
  ###########
  ## Comparison with the optimal portfolio
  #####
  
  
  opt_data = reactive({
    
    #Get backtesting data
    port_ret = bt_data()
    
    #Get optimal weights
    opt_w = opt_weights()
    
    #Extract dataframe for dates 
    from = as.Date(input$date_range[1])
    to = as.Date(input$date_range[2])
    
    opt_port(df, from, to, opt_w, port_ret)  # Comes from shiny_helper.R
    
    
  })
    
    
    
  ########
  ##  Graphs for optimal portfollios
  ########
  
  #Plot backtest compound return
  output$graph10 = renderPlotly({
    
    input$go
    
    isolate({  ### To let weights settle
      
      bt_df = opt_data()
      
      #Calculate compound return
      bt_df = bt_df %>%
        gather(key="Asset", value="Return", -date) %>%
        group_by(Asset) %>%
        arrange(date) %>%
        mutate(cumRet = cumprod(1+Return) - 1) %>%
        select(date, Asset, cumRet) %>%
        spread(key=Asset, value=cumRet)
      
      #Plot
      plot_ly(bt_df, x = ~date, y = ~Portfolio, type = "scatter", mode = "line", name = "Portfolio",
              line = list(color = "Steelblue3", width = 2), width = 700, height = 400) %>%
        add_trace(y= ~OptRet, name = "Similar Return",
                  line = list(color = "black", width = 2)) %>%
        add_trace(y= ~OptRisk, name = "Similar Risk",
                  line = list(color = "gray", width = 2)) %>%
        layout(xaxis = list(title = "", showgrid = FALSE, zeroline = TRUE, showticklabels = TRUE),
               yaxis = list(title = "", showgrid = TRUE, zeroline = TRUE, showticklabels = TRUE),
               
               legend = list(orientation = "h", x = 0.1, y=1.2),
               paper_bgcolor='rgba(0,0,0,0)',
               plot_bgcolor='rgba(0,0,0,0)',
               margin = list(b = 20, l = 20, t = 30))
    })
  })
  
  
  ## Opt Portfolio comparison table
  output$bt_table2 = renderTable(digits=4, {
    
    input$go
    
    isolate({
      #Select data
      ret_df = opt_data()
      
      ret_df = ret_df %>% rename(Same.Return=OptRet, Same.Risk = OptRisk) 
      
      rf_range = rf%>% filter(as.Date(date) >= as.Date(input$date_range[1]) & as.Date(date) <= as.Date(input$date_range[2]))
      
      
      #Calculate performance measures
      perf_df = data.frame(Measure = c("Return (annualized)","Risk (annualized)","Sharpe","Sortino","Beta","Treynor"))
      perf_df$Portfolio = unlist(calcPortMeasures(ret_df$Portfolio, ret_df$Russell2000, rf_range$rf))
      perf_df$Same.Return = unlist(calcPortMeasures(ret_df$Same.Return, ret_df$Russell2000, rf_range$rf))
      perf_df$Same.Risk = unlist(calcPortMeasures(ret_df$Same.Risk, ret_df$Russell2000, rf_range$rf))
      
      perf_df = perf_df %>% select(Measure, Portfolio, Same.Return, Same.Risk) %>% rename(Similar.Return = Same.Return,
                                                                                          Similar.Risk = Same.Risk)
      
      return (perf_df)
    })
  })
  
  
  
})

