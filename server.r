library(shiny)

shinyServer(function(input, output) {
	
  output$distPlot <- renderPlot({
	  
    plot(rnorm(input$future_rounds, 1, 2))
	})
})