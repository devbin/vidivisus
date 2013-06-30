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
    uiOutput("precontrol"),
    
    helpText(h4("Simulation parameters: ")),
    sliderInput("sld_prm1", "Param1:", min = 0, max = 10, value = 5),
    sliderInput("sld_prm2", "Param2:", min = 0, max = 10, value = 5),
    sliderInput("sld_prm3", "Param3:", min = 0, max = 10, value = 5)
  ),
  
  mainPanel(
    tabsetPanel(
      tabPanel("Plot", plotOutput("distPlot")),
      tabPanel("BarPlot", plotOutput("testPlot")),
      tabPanel("Simulation", tableOutput("simPlot"))
      )
  )
))