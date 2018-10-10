source('./func/shiny_helper.R')


shinyServer(function(input, output, session){
  
  ###############
  ##  Theory Page
  ###############
  
  # Render graphs for Theory part
  output$graph1 =renderPlotly(g1)
  output$graph2 =renderPlotly(g2)
  output$graph3 =renderPlotly(g3)
  output$graph4 =renderPlotly(g4)
  
  
  ###############
  ##  Allocation Page
  ###############
  
  #Weights
  # Initialize portfolio weights
  port_weight = reactiveValues(weight=rep(1/6, 6)) # naive diversification
  
  # If any of the sliders change, then recalculate weights to satisfy sum to 1 constraint
  observers = list(
    observeEvent(input$p1,
                 {
                   suspendMany(observers)
                   port_weight$weight = updateweight(port_weight$weight, input$p1, 1)
                   resumeMany(observers)
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
    wghtsliderInput("p1", port_weight$weight[1], label = "Russell 2000")
  })
  output$p2ui = renderUI({
    wghtsliderInput("p2", port_weight$weight[2], label = "Europian Stocks")
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
  
  
  
  
  #Min range
  observeEvent(input$date_range,{
    if(input$date_range[1] == input$date_range[2]){
      updateSliderTextInput(session,"date_range",selected = c(date_choices[1],date_choices[length(date_choices)]))
    }
  })
  
  #Allocation pie chart
  output$graph5 = renderPlotly({
  
  alloc = data.frame(wght = port_weight$weight, asset = c("Russell2000","EuropeStocks","EMStocks","Treasury","CorpBond","RealEstate"))
  
  colors = brewer.pal(6, "Blues")
  
  g5 = plot_ly(alloc, labels = ~asset, values = ~wght, type = 'pie',
               textposition = 'inside',
               textinfo = 'label+percent',
               insidetextfont = list(color = '#000'),
               hoverinfo = 'text',
               text = ~paste(round(wght,4)*100, ' %'),
               marker = list(colors = colors,
                             line = list(color = '#FFFFFF', width = 1)),
               showlegend = FALSE) %>%
    layout(xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
           yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
           width=250, height=250,
           paper_bgcolor='rgba(0,0,0,0)',
           plot_bgcolor='rgba(0,0,0,0)',
           margin = list(b = 0, l = 0, t = 0, b=0))
  
  g5
  
  
  })
  
})