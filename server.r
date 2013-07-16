library(shiny)
library(ggplot2)
library(plyr)

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

    # what we get:  annual   , quarterly , semi
    # what we want: quartery , semi      , annual
    # use a vector with the order in which things should be reordered
    treatments_in_order = c(3, 1, 2)
    df <- data.frame(l=interval, sorter=treatments_in_order)
    df = df[with(df, order(treatments_in_order)), ]
    interval = as.character(df$l)
		
		# alternative method, but I *think* it is slower
		# treatments_in_order = data.frame(lbl=c("Future quarterly treatment", "Future semiannual treatment", "Future annual treatment"))
		# interval = join(x=data.frame(lbl=interval), y=treatments_in_order, type="right")$lbl
		
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
  output$barPlot <- renderPlot({
    theplot = generateBarplot(data, input)
    
    if(!all(is.na(theplot))){
      print(theplot)
    }
  })
  
})