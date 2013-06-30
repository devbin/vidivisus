library(shiny)
library(ggplot2)

# deactivate jav_home
if(Sys.getenv("JAVA_HOME") != "")
  Sys.setenv(Java_HOME="")

library(rJava)

source(file="lib/generateplot.r")

loadData = function(){
  source(file="lib/rename-columns.R")
  
  simulationData = read.csv("testdata/wormsimtotal.txt", sep=" ")
  simulationData = renameWormdata(simulationData)
}

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
  
  
  # graph plot
  output$distPlot <- renderPlot({
    theplot = generatePlot(data, input)
    
    if(!all(is.na(theplot))){
      print(theplot)
    }
	})
  
  # bar plot
  output$testPlot <- renderPlot({
    theplot = generateBarplot(data, input)
    
    if(!all(is.na(theplot))){
      print(theplot)
    }
  })
  
  output$oldplot <- renderPlot({
    source("lib/oldplot.r")
  })
  
  # simulation plot
  output$simPlot <- renderTable({
    source("lib/simulate.r")
    data <- simulate(input$sld_prm1,input$sld_prm2,input$sld_prm3)
  })
})