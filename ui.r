library(shiny)

# Define UI for application that plots random distributions 
shinyUI(pageWithSidebar(
  
  # Application title
  headerPanel("Onchocerciasis"),
  
  sidebarPanel(
    uiOutput("treatmentIntervalUi"),
    uiOutput("pastRoundsUi"),
    uiOutput("coverage"),
    uiOutput("exposure"),
    uiOutput("IVMSet"),
    uiOutput("precontrol")
	),
  
  mainPanel(
    plotOutput("distPlot")
  )
))