library(shiny)

shinyUI(pageWithSidebar(
  
  headerPanel("Central Limit Theorem for Proportions"),
  
  sidebarPanel(
    
    sliderInput("n", 
                "Sample size:", 
                value = 20,
                min = 2, 
                max = 1000),
    br(),
    
    sliderInput("p", 
                "Population Proportion:", 
                value = .5,
                step = .01,
                min = 0, 
                max = 1),
    br(),
    
    sliderInput("k", 
                "Number of samples:", 
                value = 1000,
                min = 10, 
                max = 1000),
    
    br(),
    
    sliderInput("nbreaks", 
                "Number of bins:", 
                value = 10,
                min = 5, 
                max = 100)
  ),
  
  
  
  mainPanel(
    plotOutput("pop.dist"),
    br(),
    plotOutput("sample.dist"),
    div(h3(textOutput("num.samples")), align = "center"),
    br(),
    plotOutput("sampling.dist"),
    div(textOutput("sampling.descr"), align = "center"),
    br(),
    div(h5(textOutput("CLT.descr"), align = "center"))
  )
))