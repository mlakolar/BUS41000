library(shiny)

shinyUI(pageWithSidebar(
  
  headerPanel("Central Limit Theorem for Means"),
  
  sidebarPanel(
    radioButtons("dist", "Parent distribution (population):",
                 list("Normal" = "rnorm",
                      "Uniform" = "runif",
                      "Right skewed" = "rlnorm",
                      "Left skewed" = "rbeta")),
    br(),
    
    uiOutput("mu"),
    uiOutput("sd"),
    uiOutput("min"),
    uiOutput("max"),
    uiOutput("skew"),
    
    sliderInput("n", 
                "Sample size:", 
                value = 30,
                min = 2, 
                max = 500),
    br(),
    
    sliderInput("k", 
                "Number of samples:", 
                value = 200,
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