library(shiny)
library(ggplot2)

loadData = function(){
  source(file="lib/rename-columns.R")
  
  simulationData = read.csv("testdata/wormsimtotal.txt", sep=" ")
  simulationData = renameWormdata(simulationData)
}

generatePlot = function(data, input){
  source(file="lib/generateplot.r")
  chartx = draw(data, input)
  chartx
}

# generateBarPlot = function(data, input) {
#   source(file="lib/generateplot.r")
#   chart = barplot(1:10)
#   chart
# }

shinyServer(function(input, output) {
  data = loadData()
  
  output$treatmentIntervalUi <- renderUI({
    interval = levels(data$treatment_interval)
    checkboxGroupInput("treatment_interval", "Treatment interval:", interval ,selected=interval)
  })
  
  output$futureRoundsUi <- renderUI({
    future_rounds = data$futurerounds
    min_rounds = min(future_rounds)
    max_rounds = max(future_rounds)
    
    sliderInput("futurerounds", "Future rounds:", 
                min=min_rounds, max=max_rounds, value=max_rounds,step=1)
  })
  
  output$pastRoundsUi <- renderUI({
    past_rounds = data$pastrounds
    min_rounds = min(past_rounds)
    max_rounds = max(past_rounds)
    
    sliderInput("pastrounds", "Past rounds:", 
                min=min_rounds, max=max_rounds,value=min_rounds,step=1)
  })
  
  output$coverage <- renderUI({
    selectInput("coverage", "Coverage:",
                choices = levels(data$coverage))
  })
  
  output$exposure <- renderUI({
    selectInput("exposure", "Exposure:",
                choices = levels(data$exposure))
  })
  
  output$IVMSet <- renderUI({
    selectInput("IVMset", "IVMSet:",
                choices = levels(data$IVMset))
  })
  
  output$precontrol <- renderUI({
    selectInput("precontrol", "Precontrol:",
                choices = levels(data$precontrol))
  })
  
  output$distPlot <- renderPlot({
    theplot = generatePlot(data, input)[1]
    
    if(!all(is.na(theplot))){
      print(theplot)
    }
	})
  
  # BARPLOT!!!
  output$testPlot <- renderPlot({
    theplot = generatePlot(data, input)[2]
    
    if(!all(is.na(theplot))){
      print(theplot)
    }
  })
  
  output$oldplot <- renderPlot({
    source("lib/oldplot.r")
  })
})