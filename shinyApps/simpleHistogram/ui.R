# runApp("simpleHistogram", launch.browser = T, display.mode = "showcase")

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Histograms"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
       selectInput('dfName', 'Choose Dataset', list.files(".", pattern="*.csv")),
       selectInput('colName', 'Choose Variable', choices = c("1")),
       sliderInput("bins",
                   "Number of bins:",
                   min = 1,
                   max = 50,
                   value = 30),
       checkboxInput(inputId = "individual_obs",
                     label = strong("Show individual observations"),
                     value = FALSE)
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
        #verbatimTextOutput("summary")
        plotOutput("distPlot")
    )
  )
))
