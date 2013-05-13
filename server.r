library(shiny)

loadData = function(){
  source(file="lib/rename-columns.R")
  
  simulationData = read.csv("testdata/wormsimtotal.txt", sep=" ")
  simulationData = renameWormdata(simulationData)
}

shinyServer(function(input, output) {
  data = loadData()
  
  output$treatmentIntervalUi <- renderUI({
    checkboxGroupInput("treatmentInteval", "Treatment interval:", levels(data$treatment_interval))
  })
  
  output$futureRoundsUi <- renderUI({
    future_rounds = data$futurerounds
    min_rounds = min(future_rounds)
    max_rounds = max(future_rounds)
    
    sliderInput("future_rounds", "Future rounds:", 
                min=min_rounds, max=max_rounds, value=max_rounds,step=1)
  })
  
  output$pastRoundsUi <- renderUI({
    past_rounds = data$pastrounds
    min_rounds = min(past_rounds)
    max_rounds = max(past_rounds)
    
    sliderInput("past_rounds", "Past rounds:", 
                min=min_rounds, max=max_rounds,value=min_rounds,step=1)
  })
  
  output$coverage <- renderUI({
    selectInput("dataset", "Coverage:",
                choices = levels(data$coverage))
  })
  
  output$exposure <- renderUI({
    selectInput("dataset", "Exposure:",
                choices = levels(data$exposure))
  })
  
  output$IVMSet <- renderUI({
    selectInput("dataset", "IVMSet:",
                choices = levels(data$IVMset))
  })
  
  output$precontrol <- renderUI({
    selectInput("dataset", "Precontrol:",
                choices = levels(data$precontrol))
  })
  
  output$distPlot <- renderPlot({
    current = data$futurerounds == input$future_rounds
    plot(1:100, 101:200)
	})
})