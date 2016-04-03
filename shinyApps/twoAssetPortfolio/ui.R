library(shiny)
shinyUI(pageWithSidebar(
  
  # Title
  headerPanel("Returns on a Portfolio"),
  
  sidebarPanel(
    sliderInput("obs","Number of observations:",
                min=5,max=100,value=10,step=1),
    hr(),
    h4("Asset A"),
    sliderInput("meanA","Mean",
                min=-0.3,max=0.3,value=0.03,step=0.01),
    sliderInput("stdA","Standard deviation",
                min=0,max=0.3,value=0.05,step=0.01),
    h4("Asset B"),
    sliderInput("meanB","Mean",
                min=-0.3,max=0.3,value=0.03,step=0.01),
    sliderInput("stdB","Standard deviation",
                min=0,max=0.3,value=0.05,step=0.01),
    hr(),
    sliderInput("rho","Correlation Between Returns",
                min=-1,max=1,value=0,step=0.05),    
    sliderInput("w","Weight on Asset A",
                min=0,max=1,value=0.5,step=0.01),
    hr(),
    sliderInput("nbins","Number of bins",
                min=3,max=30,value=5,step=1),
    checkboxInput("showLabels", "Show portfolio returns", value=T)
  ),
  
  # plot
  mainPanel(
    plotOutput("outScatter"),
    plotOutput("outHist"),
    plotOutput("outScatter1")
  )
))