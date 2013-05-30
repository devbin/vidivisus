library(ggplot2)
loadData = function(){
  source(file="lib/rename-columns.R")
  
  simulationData = read.csv("testdata/wormsimtotal.txt", sep=" ")
  simulationData = renameWormdata(simulationData)
}

plotdata = function(data, input)
{ 
  filter = NA
  filter = data$exposure %in% input$exposure  
  filter = filter & data$precontrol %in% input$precontrol
  filter = filter & data$IVMset %in% input$IVMset 
  filter = filter &data$coverage%in%input$coverage
  filter = filter & data$treatment_interval %in% input$treatment_interval
  
  pastrounds = input$pastrounds
  dataframe_pastrounds= data.frame()
  repeat
  {
    if(pastrounds == 0)
    {
      break;
    }
    
    filter_pastrounds = filter & data$futurerounds == 0
    filter_pastrounds = filter_pastrounds & data$pastrounds == pastrounds
    pastrounds = pastrounds -1
    filterd = subset(data, filter_pastrounds)
    
    dataframe_pastrounds = rbind(dataframe_pastrounds, filterd)
  }
  
  filter_futurerounds = filter & data$pastrounds == pastrounds
  dataframe_futurerounds = subset(data, filter_futurerounds)
  
  plotdata = rbind(dataframe_pastrounds, dataframe_futurerounds)
}