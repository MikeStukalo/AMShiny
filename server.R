library(plotly)


shinyServer(function(input, output, session){
  
  # Render graphs for Theory part
  output$graph1 <-renderPlotly(g1)
  output$graph2 <-renderPlotly(g2)
  output$graph3 <-renderPlotly(g3)
  output$graph4 <-renderPlotly(g4)
  
  # TEMP
  output$res <- renderPrint(input$date_range)
  
  #Min range
  observeEvent(input$date_range,{
    if(input$date_range[1] == input$date_range[2]){
      updateSliderTextInput(session,"date_range",selected = c(date_choices[1],date_choices[length(date_choices)]))
    }
  })
  
})