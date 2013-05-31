plotdata = function(data, input)
{ 
  filter = NA
  filter = data$exposure %in% input$exposure  
  filter = filter & data$precontrol %in% input$precontrol
  filter = filter & data$IVMset %in% input$IVMset 
  filter = filter & data$coverage %in% input$coverage
  
  pastrounds = input$pastrounds
  dataframe_pastrounds= data.frame()
  
  filter_pastrounds = filter & data$futurerounds == 0
  filter_pastrounds = filter_pastrounds & data$pastrounds %in% seq(0, pastrounds, 1)
  filter_pastrounds = filter_pastrounds & data$treatment_interval == "Future annual treatment"

  dataframe_pastrounds = subset(data, filter_pastrounds)
  dataframe_pastrounds$treatment_interval = "Past rounds"
  
  pastrounds = input$pastrounds
  filter_futurerounds = filter & data$pastrounds == pastrounds
  filter_futurerounds = filter_futurerounds & data$treatment_interval %in% input$treatment_interval
  dataframe_futurerounds = subset(data, filter_futurerounds)
  
  plotdata = rbind(dataframe_pastrounds, dataframe_futurerounds)
}

draw = function(data, input){
  
  plot = NA
  if(length(input$treatment_interval) != 0){
    filtered = plotdata(data, input)
    source("lib//calculate_coords.R")
  
    finaldata = calculate_coords(filtered, input$pastrounds)
    lim_x=max(finaldata$x)
    lim_y=max(finaldata$y)
    
    begin_x = 2013 - max(finaldata$pastrounds)
    end_x = 2013 + max(finaldata$futurerounds)
    begin_y = 0
    end_y = 100
    
    plot = ggplot(finaldata, aes(x=x, y=y, colour=treatment_interval)) + geom_line() +
        scale_x_discrete(breaks=seq(0 , lim_x, 8), labels=seq(begin_x, end_x, 2)) +
        scale_y_discrete(breaks=seq(0 , lim_y, lim_y/end_y*5), labels=seq(begin_y, end_y, 5)) +
        xlab("Years") + ylab("Elimination probability")
  }
  plot
}