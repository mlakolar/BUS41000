library(shiny)
shinyUI(pageWithSidebar(
  
  # Title
  headerPanel("Simulating Data with Correlation"),
  
  sidebarPanel(
    sliderInput("obs","Number of observations:",
                min=5,max=200,value=50,step=5),
    sliderInput("rho","Correlation Coefficient",
                min=-1,max=1,value=0,step=0.05)    
  ),
  # GGPLOT
  mainPanel(
    plotOutput("distPlot")
  )
))