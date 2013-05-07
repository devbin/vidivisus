library(shiny)

# Define UI for application that plots random distributions 
shinyUI(pageWithSidebar(
  
  # Application title
  headerPanel("Onchocerciasis"),
  
  sidebarPanel(
	  selectInput("dataset", "Treatment period:", 
	                  choices = c("Annual", "Semi-Annual", "Quarterly")),
					  
	  sliderInput("future_rounds", "Future rounds:", 
	                  min=0, max=20, value=20),

	  sliderInput("past_rounds", "Past rounds:", 
	                  min=0, max=20, value=0),
					  
	  selectInput("dataset", "Future coverage:", 
	                  choices = paste(1:100, NULL, sep="%"), selected = "50%" ),
					  
	  selectInput("dataset", "Past coverage:", 
	                  choices =paste(1:100, NULL, sep="%"), selected = "50%" )
	),
  
  mainPanel(
    plotOutput("distPlot")
    
  )
))