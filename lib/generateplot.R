draw = function(data, input){
  filter = NA
  filter = data$exposure %in% input$exposure  
  filter = filter & data$precontrol %in% input$precontrol
  filter = filter & data$IVMset %in% input$IVMset 
  filter = filter &data$coverage%in%input$coverage
  filter = filter & data$pastrounds%in%input$pastrounds
  #filter = filter & data$futurerounds%in%input$futurerounds
  filter = filter & data$treatment_interval %in% input$treatment_interval
  filterd = subset(data, filter)
  
  plot = NA
  if(length(input$treatment_interval) != 0){
     plot = ggplot(filterd, aes(x=futurerounds, y=elimination_probability, colour=treatment_interval))+ geom_line()
  }
  
  plot
}